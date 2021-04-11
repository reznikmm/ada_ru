with League.Calendars;
with League.Stream_Element_Vectors;
with League.Strings;

package Mails is

   type Mail is tagged record
      From        : League.Strings.Universal_String;
      Date        : League.Calendars.Date_Time;
      In_Reply_To : League.Strings.Universal_String;
      Message_Id  : League.Strings.Universal_String;
      Subject     : League.Strings.Universal_String;
      Text        : League.Strings.Universal_String;
      Is_Flowed   : Boolean := False;
   end record;

   function Read_Mail
     (Data : League.Stream_Element_Vectors.Stream_Element_Vector)
       return Mail;

end Mails;
