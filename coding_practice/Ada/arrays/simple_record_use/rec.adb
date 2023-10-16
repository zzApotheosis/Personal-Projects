--
-- Record types are used to represent things having several attributes.
-- They are like classes, have no methods or access controls.
--
-- This program reads in a list of flights, then lists which of the flights
-- leave any particular airport.  The input is a list of flights, one
-- per line, number, departs, destination.  This is ended by a -1 line,
-- the followed by a list of airports.  The flights departing each of
-- these airports are listed.  This ends with the word STOP.
--
with Text_IO;
with Gnat.Io; use Gnat.Io;
procedure Rec is
   -- Describe flights.
   type Airport_Or_Stop is ( ORD, DCA, STL, ATL, LAX, STOP );
   subtype Airport is Airport_Or_Stop range ORD .. LAX;
   type Flight is record
      Number: Integer;
      From, Dest: Airport;
   end record;

   -- I/O on the legitimate airport codes.
   package Airport_IO is new Text_Io.Enumeration_IO(Airport);
   use Airport_IO;

   -- I/O on the codes or the STOP string.  We cannot "use" this new
   -- class, because it ambiguates the Airport_IO calls.  When we do use
   -- it (one place), we say Airport_Or_Stop_IO.Get().
   package Airport_Or_Stop_IO is new Text_Io.Enumeration_IO(Airport_Or_Stop);

   -- List of flights.
   N: constant := 25;
   Flights: array(1..N) of Flight;
   N_Flights: Integer;

   I: Integer;                  -- Loop index.
   Flt: Integer;                -- Input flight number.
   Leave: Airport_Or_Stop;      -- Input city.
   Found: Boolean;              -- Found a flight out.
begin
   -- Read in the list.
   I := 1;
   loop
      -- Get a flight number, exit for -1.
      Get(Flt);
      exit when Flt = -1;

      -- Rest of the flight.
      Flights(I).Number := Flt;
      Get(Flights(I).From);
      Get(Flights(I).Dest);

      I := I + 1;
   end loop;
   N_Flights := I - 1;

   -- Find connections.
   loop
      -- Where are we leaving from?
      Airport_Or_Stop_IO.Get(Leave);
      exit when Leave = STOP;
      Put("From ");
      Put(Leave);
      Put_Line(":");

      -- Can we leave from there?
      Found := False;
      for I in 1..N_Flights loop
         if Flights(I).From = Leave then
            -- Print the flight
            Put("  Flight ");
            Put(Flights(I).Number);
            Put(" to ");
            Put(Flights(I).Dest);
            New_Line;

            -- Record that we found this.
            Found := True;
         end if;
      end loop;

      -- Print a message if no flights out were found.
      if not Found then
         Put("  No flights depart ");
         Put(Leave);
         Put_Line(".");
      end if;

      New_Line;
   end loop;
end Rec;
