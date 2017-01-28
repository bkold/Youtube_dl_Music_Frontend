package Call is

	procedure Program_Call (Command : in String) with Import, Convention=>C, Link_Name=>"system";

	procedure Download (URL : in String);

	procedure Search_Play (File_Name, Artist, Title : in String);

	procedure Search_Time (File_Name, URL : in String);

	procedure Search_Tube (File_Name, Artist, Title : in String);

end Call;