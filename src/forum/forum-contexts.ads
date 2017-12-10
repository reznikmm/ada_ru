with Forum.Forums;
with Forum.Posts;
with Forum.Topics;
with Forum.Users;

package Forum.Contexts is
   type Context is limited record
      Forums : aliased Forum.Forums.Container (Context'Unchecked_Access);
      Posts  : aliased Forum.Posts.Container (Context'Unchecked_Access);
      Topics : aliased Forum.Topics.Container (Context'Unchecked_Access);
      Users  : aliased Forum.Users.Container (Context'Unchecked_Access);
   end record;

   type Context_Access is access all Context;

end Forum.Contexts;
