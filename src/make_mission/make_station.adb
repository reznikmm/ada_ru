with League.Application;
with League.Strings;
with League.JSON.Arrays;
with League.JSON.Objects;
with League.JSON.Values;

with IO;
with Make_Mission;

procedure Make_Station is
   use type League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   Station_File : constant League.Strings.Universal_String :=
     League.Application.Arguments.Element (1);

   Station  : League.JSON.Objects.JSON_Object;
   Missions : League.JSON.Arrays.JSON_Array;
   List     : League.JSON.Arrays.JSON_Array;

begin
   IO.Read_JSON (Station_File, Station);
   Missions := Station.Value (+"missions").To_Array;

   for J in 1 .. Missions.Length loop
      declare
         Object : League.JSON.Objects.JSON_Object;
      begin
         Make_Mission ("missions/" & Missions (J).To_String, Station, Object);
         List.Append (Object.To_JSON_Value);
      end;
   end loop;

   declare
      Slug : constant League.Strings.Universal_String :=
        Station.Value (+"slug").To_String;
   begin
      Station.Insert (+"missions", List.To_JSON_Value);

      IO.Expand_Template
        (File_Name => +"make_mission/station.xhtml",
         Context   => Station,
         Out_Name  => "/tmp/game/" & Slug & ".html");
   end;
end Make_Station;
