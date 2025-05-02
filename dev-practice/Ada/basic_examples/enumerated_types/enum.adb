--
-- The enumerated types.
--
with Gnat.Io; use Gnat.Io;
procedure Enum is
   type Day_Type is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   subtype Week_Days is Day_Type range Mon .. Fri;
   Day: Day_Type;
   Sdays: array (Day_Type) of String(1..3) :=
     ("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat");

   -- Notice name conflict on Sat.  Ada doesn't mind.
   type Declined is (Stood, Sat, Fall);
   Declination: Declined;
begin
   -- Print out all the days
   for Day in Day_Type loop
      Put(Sdays(Day) & " ");
   end loop;
   New_Line;

   -- Print out all the days another way.
   for Day in Sun .. Sat loop
      Put(Sdays(Day) & " ");
   end loop;
   New_Line;

   -- Print out the week days yet another way.
   Day := Week_Days'First;
   while Day <= Week_Days'Last loop
      Put(Sdays(Day) & " ");
      Day := Week_Days'Succ(Day);
   end loop;
   New_Line;

   -- Print out the days of a MWF class
   for Day in Week_Days loop
      if (Week_Days'Pos(Day) - Week_Days'Pos(Mon)) mod 2 = 0 then
         Put(Sdays(Day) & " ");
      end if;
   end loop;
   New_Line;

   Day := Sat;
   Declination := Sat;
   Put(Day_Type'Pos(Day));
   Put(" ");
   Put(Declined'Pos(Declination));
   New_Line;
end Enum;
