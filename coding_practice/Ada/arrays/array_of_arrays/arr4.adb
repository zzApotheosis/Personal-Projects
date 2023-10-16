--
-- An array of array (which is different from a 2-d array).
--
with Gnat.Io; use Gnat.Io;
procedure Arr4 is
   type CS is (G_Wiggins, M_Wiggins, Bennet, Woodall);
   type CSMap is array (CS) of Integer;
   A: array (1..3) of CSMap :=
     ((4, 8, 17, 34),
      (9, 11, 3, -2),
      (6, 21, 2, 18));
   I: Integer;
   P: CS;
   Row: CSMap;
begin
   -- Print it the usual way.
   for I in 1..3 loop
      for P in CS loop
         Put(A(I)(P));
         Put(" ");
      end loop;
      New_Line;
   end loop;
   New_Line;

   -- Print it the inverted way.
   for P in CS loop
      for I in 1..3 loop
         Put(A(I)(P));
         Put(" ");
      end loop;
      New_Line;
   end loop;
   New_Line;

   -- Print it once more.  Note that you cannot do this with a
   -- 2-d array because the assignment Row := A(I) is illegal.
   -- (Mostly the reference A(I) is illegal.)
   for I in 1..3 loop
      Row := A(I);   -- Copies one row of A into Row.
      for P in CS loop
         Put(Row(P));
         Put(" ");
      end loop;
      New_Line;
   end loop;

end Arr4;
