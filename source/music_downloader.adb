with GNAT.Regpat; use GNAT.Regpat;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings;

with String_Manipulation;
with Call;

procedure Music_Downloader is

	type Song_Type is record
		Time : Natural;
		Artist : String(1..32);
		Title : String(1..64);
	end record;

	protected type Protected_File is
		procedure Open_For_Reading (Name : in String);
		procedure Close_File;
		function Get_Line return String;
		function End_Of_File return Boolean;
	private
		Input_File : File_Type;
	end Protected_File;

	protected body Protected_File is

		procedure Open_For_Reading (Name : in String) is
		begin
			Open(Input_File, In_File, Name);
		end Open_For_Reading;

		procedure Close_File is
		begin
			Close(Input_File);
		end Close_File;

		function Get_Line return String is
		begin
			return Get_Line(Input_File);
		end Get_Line;

		function End_Of_File return Boolean is
		begin
			return End_Of_File(Input_File);
		end End_Of_File;

	end Protected_File;

	User_Input_File : Protected_File;

	procedure Initialize_Program is
		use Ada.Command_Line;
	begin
		Create_Directory("~temp");
		Create_Directory("~fails");
		delay 0.5;
		User_Input_File.Open_For_Reading(Argument(1));
	end Initialize_Program;

	procedure Terminate_Program is
	begin
		User_Input_File.Close_File;
		Delete_Directory("~temp");
		Delete_Directory("~fails");
	end Terminate_Program;

	function Search_Regexp (Pattern : in String; Search_In : in String) return Match_Array is
		Re : constant Pattern_Matcher := Compile (Pattern, Case_Insensitive);
		Matches : Match_Array (0..2);
	begin
		Match (Re, Search_In, Matches);
		return Matches;
	end Search_Regexp;

	procedure Wait (File : in String) is
	begin
		while not Exists(File) loop
			delay(1.0);
		end loop;
		delay(1.0);
	end Wait;

	procedure Get_Song (Data : in out Song_Type; ID : in Natural) is
		use Ada.Strings.Fixed;
		use Call;
		use String_Manipulation;
		Next_URL : String(1..64);
		Match_Indices : Match_Array(0..2) := (others=>No_Match);
		Data_File : File_Type;
		First_File : constant String := "~temp/First_Pull" & Integer'Image(-ID);
		Second_File : constant String := "~temp/Second_Pull" & Integer'Image(-ID);
		Youtube_File : constant String := "~temp/Youtube_Pull" & Integer'Image(-ID);
	begin

		Search_Play(First_File, Data.Artist, Data.Title);
		Wait("./" & First_File);

		Open(Data_File, In_File, "./" & First_File);
		while Match_Indices(0) = No_Match and not End_Of_File(Data_File) loop
			declare
				Pulled_Line : constant String := Get_Line(Data_File);
			begin
				Match_Indices := Search_Regexp("song-.*?href=""(.*?);", Pulled_Line);
				if Match_Indices(0) /= No_Match then
					Move(Pulled_Line(Match_Indices(1).First..Match_Indices(1).Last), Next_URL);
				end if;
			end;
		end loop;
		Close(Data_File);

		if Match_Indices(0) /= No_Match then

			Match_Indices(0) := No_Match;
			Search_Time(Second_File, Next_URL);
			Wait("./" & Second_File);

			Open(Data_File, In_File, "./" & Second_File);
			while Match_Indices(0) = No_Match and not End_Of_File(Data_File) loop
				declare
					Pulled_Line : constant String := Get_Line(Data_File);
				begin
					Match_Indices := Search_Regexp("<th>Songs</th>.*?" & Quote(To_Spaces(Data.Title)) & ".*?aria-label.*?>(.*?)<", Pulled_Line);
					if Match_Indices(0) /= No_Match then
						Data.Time := Get_Time(Pulled_Line(Match_Indices(1).First..Match_Indices(1).Last));
					end if;
				end;
			end loop;
			Close(Data_File);

			if Match_Indices(0) /= No_Match then

				Match_Indices(0) := No_Match;
				Search_Tube(Youtube_File, Data.Artist, Data.Title);
				Wait("./" & Youtube_File);

				Open(Data_File, In_File, "./" & Youtube_File);
				while Match_Indices(0) = No_Match and not End_Of_File(Data_File) loop
					declare
						Pulled_Line : constant String := Get_Line(Data_File);
						Pulled_Time : Natural;
					begin
						Match_Indices := Search_Regexp("href=""(.*?)class.*?Duration: (.*?)\.", Pulled_Line);
						if Match_Indices(0) /= No_Match then
							Pulled_Time := Get_Time(Pulled_Line(Match_Indices(2).First..Match_Indices(2).Last));
							if Pulled_Time in Data.Time-5..Data.Time+5 then
								Download(Pulled_Line(Match_Indices(1).First..Match_Indices(1).Last));
							else
								Match_Indices(0) := No_Match;
							end if;
						end if;
					end;
				end loop;
				Close(Data_File);

				if Match_Indices(0) = No_Match then
					Put_Line("There was a problem finding a suitable video for '" & To_Spaces(Data.Title) &
						"' by " & To_Spaces(Data.Artist) & " on youtube");
					--DEBUG
					Copy_File("./" & Youtube_File, "./~fails/" & Data.Artist & Data.Title & " Youtube");
					Copy_File("./" & Second_File, "./~fails/" & Data.Artist & Data.Title & " Second");
					Copy_File("./" & First_File, "./~fails/" & Data.Artist & Data.Title & " First");
				end if;

				Delete_File("./" & Youtube_File);
			else
				Put_Line("There was a problem finding '" & To_Spaces(Data.Title) &
					"' by " & To_Spaces(Data.Artist) & " during the second pull");
				--DEBUG
				Copy_File("./" & Second_File, "./~fails/" & Data.Artist & Data.Title & " Second");
				Copy_File("./" & First_File, "./~fails/" & Data.Artist & Data.Title & " First");
			end if;

			Delete_File("./" & Second_File);
		else
			Put_Line("There was a problem finding any record of '" & To_Spaces(Data.Title) &
				"' by " & To_Spaces(Data.Artist));
			--DEBUG
			Copy_File("./" & First_File, "./~fails/" & Data.Artist & Data.Title & " First");
		end if;

		Delete_File("./" & First_File);

	exception
		when Error : Others =>
			Ada.Text_IO.Put("Unexpected Exception :  Failed on " &
				To_Spaces(Data.Title) & " by " &
				To_Spaces(Data.Artist) & " with " &
				Ada.Exceptions.Exception_Information(Error));
			if Is_Open(Data_File) then
				Close(Data_File);
			end if;
			if Exists("./" & Youtube_File) then
				Delete_File("./" & Youtube_File);
			end if;
			if Exists("./" & Second_File) then
				Delete_File("./" & Second_File);
			end if;
			if Exists("./" & First_File) then
				Delete_File("./" & First_File);
			end if;

	end Get_Song;

	task type Downloader_Thread is
		entry Start (Set_ID : in Natural);
	end Downloader_Thread;

	task body Downloader_Thread is
		ID : Natural;
	begin
		accept Start (Set_ID : in Natural) do
			ID := Set_ID;
		end Start;
		while not User_Input_File.End_Of_File loop
			declare
				use Ada.Strings;
				use Ada.Strings.Fixed;
				New_Song : Song_Type;
				Line : constant String := User_Input_File.Get_Line;
			begin
				Move(Line(Line'First..Index(Line, "|")-1), New_Song.Artist, Right, Left);
				Move(Line(Index(Line, "|")+1..Line'Last), New_Song.Title, Right, Left);
				Translate(New_Song.Artist, String_Manipulation.Space_to_Dash_Map);
				Translate(New_Song.Title, String_Manipulation.Space_to_Dash_Map);
				Get_Song(New_Song, ID);

			end;
		end loop;
	end Downloader_Thread;

begin

	Initialize_Program;

	declare
		T1 : Downloader_Thread;
		T2 : Downloader_Thread;
		T3 : Downloader_Thread;
		T4 : Downloader_Thread;
	begin
		T1.Start(1);
		T2.Start(2);
		T3.Start(3);
		T4.Start(4);
	end;

	Terminate_Program;

end Music_Downloader;