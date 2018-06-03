function League.Calendars.From_Seconds
  (Value : Natural) return League.Calendars.Time is
begin
   return Time (Value) * 10E7;
end League.Calendars.From_Seconds;
