with String_Manipulation; use String_Manipulation;
with Ada.Text_IO; use Ada.Text_IO;

package body Call is

	procedure Download (URL : in String) is
		Command : constant String := "youtube-dl ""https://www.youtube.com" & URL & "-q -x --audio-quality 0 --audio-format mp3" & ASCII.NUL;
	begin
		-- Put_Line(Command);
		Program_Call(Command);
	end Download;

	procedure Search_Play (File_Name, Artist, Title : in String) is
		Command : constant String := "curl -s -o " & File_Name & " ""https://play.google.com/store/search?q=" & To_Underscore(Title) & "-" & To_Underscore(Artist) & "&c=music&hl=en""" & ASCII.NUL;
	begin
		-- Put_Line(Command);
		Program_Call(Command);
	end Search_Play;

	procedure Search_Time (File_Name, URL : in String) is
		Command : constant String := "curl -s -o " & File_Name & " ""https://play.google.com" & URL & """" & ASCII.NUL;
	begin
		-- Put_Line(Command);
		Program_Call(Command);
	end Search_Time;

	procedure Search_Tube (File_Name, Artist, Title : in String) is
		Command : constant String := "curl -s -o " & File_Name & " ""https://www.youtube.com/results?search_query=" & Remove_Plus(Artist) & "-" & Remove_Plus(Title) & """" & ASCII.NUL;
	begin
		-- Put_Line(Command);
		Program_Call(Command);
	end Search_Tube;

end Call;