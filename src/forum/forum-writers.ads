with League.Holders;

package Forum.Writers is
   procedure Write_Forum_Index (Value : League.Holders.Holder);

   procedure Write_Forum_Page
     (Forum : League.Holders.Holder;
      Page  : League.Holders.Holder);

   procedure Write_Topic_Page
     (Forum : League.Holders.Holder;
      Topic : League.Holders.Holder;
      Page  : League.Holders.Holder);

end Forum.Writers;
