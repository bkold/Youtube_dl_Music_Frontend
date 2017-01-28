with Ada.Strings.Maps;

package String_Manipulation is

	Space_to_Dash_Map : constant Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.To_Mapping(
		From=>" ", 
		To=>"-");
	Dash_to_Space_Map : constant Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.To_Mapping(
		From=>"-", 
		To=>" ");
	Dash_to_Under_Map : constant Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.To_Mapping(
		From=>"-", 
		To=>"_");

	function Remove_Plus (Line : in String) return String;

	function Get_Time (Input_String : in String) return Natural;

	function Truncated_String_Index (Input : in String) return Natural;

	function To_Spaces (Input : in String) return String;

	function To_Underscore (Input : in String) return String;

end String_Manipulation;