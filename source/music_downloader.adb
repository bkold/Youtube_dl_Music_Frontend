with GNAT.Regpat; use GNAT.Regpat;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Command_Line;
with Ada.Strings.Fixed; 
with Ada.Strings; 
with Ada.Strings.Maps;

procedure Music_Downloader is

	procedure Program_Call (Command : in String) with Import, Convention=>C, Link_Name=>"system";

	type Song_Type is record
		Time : Natural;
		Artist : String(1..32);
		Title : String(1..64);
	end record;

	Space_to_Dash_Map : constant Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.To_Mapping(
		From=>" ", 
		To=>"-");
	Dash_to_Space_Map : constant Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.To_Mapping(
		From=>"-", 
		To=>" ");
	Dash_to_Under_Map : constant Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.To_Mapping(
		From=>"-", 
		To=>"_");

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
		delay 0.5;
		User_Input_File.Open_For_Reading(Argument(1));
	end Initialize_Program;

	procedure Terminate_Program is
	begin
		User_Input_File.Close_File;
		Delete_Directory("~temp");
	end Terminate_Program;

	function Search_Regexp (Pattern : in String; Search_In : in String) return Match_Array is
		Re : constant Pattern_Matcher := Compile (Pattern);
		Matches : Match_Array (0..2);
	begin
		Match (Re, Search_In, Matches);
		return Matches;
	end Search_Regexp;

	function Get_Time (Input_String : in String) return Natural is
		use Ada.Strings.Fixed;
		Time : Natural;
	begin
		Time := Natural'Value(Input_String(Input_String'First .. Index(Input_String, ":", Input_String'First)-1)) * 60;
		Time := Time + Natural'Value(Input_String(Index(Input_String, ":", Input_String'First)+1 .. Input_String'Last));
		return Time;
	end Get_Time;

	procedure Download (URL : in String) is
	begin
		Program_Call("youtube-dl " & Character'Val(34) & "https://www.youtube.com" & URL & "-q -x --audio-quality 0 --audio-format mp3" & ASCII.NUL);
	end Download;

	procedure Wait (File : in String) is
	begin
		while not Exists(File) loop
			delay(1.0);
		end loop;
		delay(1.0);
	end Wait;

	procedure Get_Song (Data : in out Song_Type; ID : in Natural) is
		use Ada.Strings.Fixed;
		Next_URL : String(1..64);
		Match_Indices : Match_Array(0..2) := (others=>No_Match);
		Data_File : File_Type;
		First_File : constant String := "~temp/First_Pull" & Integer'Image(-ID);
		Second_File : constant String := "~temp/Second_Pull" & Integer'Image(-ID);
		Youtube_File : constant String := "~temp/Youtube_Pull" & Integer'Image(-ID);
	begin

		-- Put_Line("curl -s -o " & First_File & " " & Character'Val(34) & "https://play.google.com/store/search?q=" & Translate(Data.Artist, Dash_to_Under_Map) & "-" & Translate(Data.Title, Dash_to_Under_Map) & "&c=music&hl=en" & Character'Val(34) & ASCII.NUL);
		Program_Call("curl -s -o " & First_File & " " & Character'Val(34) & "https://play.google.com/store/search?q=" & Translate(Data.Artist, Dash_to_Under_Map) & "-" & Translate(Data.Title, Dash_to_Under_Map) & "&c=music&hl=en" & Character'Val(34) & ASCII.NUL);
		Wait("./" & First_File);

		Open(Data_File, In_File, "./" & First_File);
		while Match_Indices(0) = No_Match and not End_Of_File(Data_File) loop
			declare
				Pulled_Line : constant String := Get_Line(Data_File);
			begin
				Match_Indices := Search_Regexp("song-.*?href=" & Character'Val(34) & "(.*?);", Pulled_Line);
				if Match_Indices(0) /= No_Match then
					-- Put_Line("MATCHED!!!");
					-- Put_Line(Pulled_Line(Match_Indices.First .. Match_Indices.Last));
					Move(Pulled_Line(Match_Indices(1).First .. Match_Indices(1).Last), Next_URL);
				end if;
			end;
		end loop;		
		Close(Data_File);

		if Match_Indices(0) /= No_Match then

			Match_Indices(0) := No_Match;
			-- Put_Line("curl -s -o " & Second_File & " " & Character'Val(34) & "https://play.google.com" & Next_URL & Character'Val(34) & ASCII.NUL);
			Program_Call("curl -s -o " & Second_File & " " & Character'Val(34) & "https://play.google.com" & Next_URL & Character'Val(34) & ASCII.NUL);
			Wait("./" & Second_File);

			Open(Data_File, In_File, "./" & Second_File);
			while Match_Indices(0) = No_Match and not End_Of_File(Data_File) loop
				declare
					Pulled_Line : constant String := Get_Line(Data_File);
				begin
					Match_Indices := Search_Regexp("<th>Songs</th>.*?" & Translate(Data.Title(Data.Title'First..Index(Data.Title, "--")-1), Dash_to_Space_Map) & ".*?aria-label.*?>(.*?)<", Pulled_Line);
					if Match_Indices(0) /= No_Match then
						-- Put_Line("MATCHED!!!");
						Data.Time := Get_Time(Pulled_Line(Match_Indices(1).First..Match_Indices(1).Last));
						-- Put_Line(Natural'Image(Data.Time));
					end if;
				end;
			end loop;
			Close(Data_File);

			if Match_Indices(0) /= No_Match then

				Match_Indices(0) := No_Match;
				-- Put_Line("curl -s -o " & Youtube_File & " " & Character'Val(34) & "https://www.youtube.com/results?search_query=" & Data.Artist & "-" & Data.Title & Character'Val(34) & ASCII.NUL);
				Program_Call("curl -s -o " & Youtube_File & " " & Character'Val(34) & "https://www.youtube.com/results?search_query=" & Data.Artist & "-" & Data.Title & Character'Val(34) & ASCII.NUL);
				Wait("./" & Youtube_File);

				Open(Data_File, In_File, "./" & Youtube_File);
				while Match_Indices(0) = No_Match and not End_Of_File(Data_File) loop
					declare
						Pulled_Line : constant String := Get_Line(Data_File);
						Pulled_Time : Natural;
					begin
						Match_Indices := Search_Regexp("href=" & Character'Val(34) & "(.*?)class.*?Duration: (.*?)\.", Pulled_Line);
						if Match_Indices(0) /= No_Match then
							Pulled_Time := Get_Time(Pulled_Line(Match_Indices(2).First..Match_Indices(2).Last));
							if Pulled_Time in Data.Time-5..Data.Time+5 then
								-- Put_Line("MATCHED!!!");
								-- Put_Line(Pulled_Line(Match_Indices(1).First..Match_Indices(1).Last));
								-- Put_Line(Natural'Image(Pulled_Time));
								Download(Pulled_Line(Match_Indices(1).First..Match_Indices(1).Last));
							else
								Match_Indices(0) := No_Match;
							end if;
						end if;
					end;
				end loop;
				Close(Data_File);

				Delete_File("./" & Youtube_File);
			else
				Put_Line("There was a problem finding '" & Data.Title & "' by " & Data.Artist & "during the second pull");
			end if;

			Delete_File("./" & Second_File);
		else
			Put_Line("There was a problem finding any record of '" & Data.Title & "' by " & Data.Artist);
		end if;

		Delete_File("./" & First_File);

	exception 
		when others =>
			Put_Line("Something Happened");
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
				Translate(New_Song.Artist, Space_to_Dash_Map);
				Translate(New_Song.Title, Space_to_Dash_Map);
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