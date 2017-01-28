with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body String_Manipulation is

	function Remove_Plus (Line : in String) return String is
		Plus_Index : constant Natural := Index(Line, "+", Line'First);
	begin
		if Plus_Index /= 0 then
			return Replace_Slice(Line, Plus_Index, Plus_Index, "and");
		else
			return Line;
		end if;
	end Remove_Plus;

	function Get_Time (Input_String : in String) return Natural is
		Time : Natural;
	begin
		Time := Natural'Value(Input_String(Input_String'First .. Index(Input_String, ":", Input_String'First)-1)) * 60;
		Time := Time + Natural'Value(Input_String(Index(Input_String, ":", Input_String'First)+1 .. Input_String'Last));
		return Time;
	exception
		when Constraint_Error =>
			return Natural'Last-5;
	end Get_Time;

	function Truncated_String_Index (Input : in String) return Natural is
	begin
		return Index(Input, "--")-1;
	end Truncated_String_Index;

	function To_Spaces (Input : in String) return String is
	begin
		return Translate(Input(Input'First..Truncated_String_Index(Input)), Dash_to_Space_Map);
	end To_Spaces;

	function To_Underscore (Input : in String) return String is
	begin
		return Translate(Input(Input'First..Truncated_String_Index(Input)), Dash_to_Under_Map);
	end To_Underscore;

	
end String_Manipulation;