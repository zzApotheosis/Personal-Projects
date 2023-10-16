--
-- Just a 2-D array.
--
with Gnat.Io; use Gnat.Io;
procedure Arr3 is
   type CS is (G_Wiggins, M_Wiggins, Bennet, Woodall);
   A: array (1..3, CS) of Integer :=
     ((4, 8, 17, 34),
      (9, 11, 3, -2),
      (6, 21, 2, 18));
   I: Integer;
   P: CS;
begin
   -- Print it the usual way.
   for I in 1..3 loop
     for P in CS loop
       Put(A(I,P));
       Put(" ");
     end loop;
     New_Line;
   end loop;
   New_Line;

   -- Print it the inverted way.
   for P in CS loop
      for I in 1..3 loop
       Put(A(I,P));
       Put(" ");
     end loop;
     New_Line;
   end loop;
end Arr3;
