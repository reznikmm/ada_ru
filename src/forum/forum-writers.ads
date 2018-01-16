with League.Holders;

package Forum.Writers is
   procedure Write_Forum_Atom
     (Root  : String;
      Value : League.Holders.Holder);

   procedure Write_Forum_Index
     (Root  : String;
      Value : League.Holders.Holder);

   procedure Write_Forum_Page
     (Root  : String;
      Forum : League.Holders.Holder;
      Page  : League.Holders.Holder);

   procedure Write_Topic_Page
     (Root  : String;
      Forum : League.Holders.Holder;
      Topic : League.Holders.Holder;
      Page  : League.Holders.Holder);

end Forum.Writers;
