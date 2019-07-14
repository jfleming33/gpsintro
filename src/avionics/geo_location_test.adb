with Geo_Location;
with Ada.Text_IO;

procedure Geo_Location_Test is
   type Location_Array is array (Integer range <>) of Geo_Location.Geo_Location_Type; 
   Location : Location_Array(1 .. 10);
   New_Location : Geo_Location.Geo_Location_Type;

begin
   Ada.Text_IO.Put_Line("");   
   Location(1) := Geo_Location.Initialize(Geo_Location.Bounded_Location.To_Bounded_String("Princeton, NJ"), 40.366633, -74.640832);
   Location(2) := Geo_Location.Initialize(Geo_Location.Bounded_Location.To_Bounded_String("Ithica, NY"), 42.443087, -76.488707);
   Location(3) := Geo_Location.Initialize(Geo_Location.Bounded_Location.To_Bounded_String("Montgomery, AL"), 32.3668, -86.3000);
   Location(4) := Geo_Location.Initialize(Geo_Location.Bounded_Location.To_Bounded_String("Warner Robins, GA"), 32.6130, -83.6242);
   Location(5) := Geo_Location.Initialize(Geo_Location.Bounded_Location.To_Bounded_String("Anchorage, AK"), 61.2181, -149.9003);
   Location(6) := Geo_Location.Initialize(Geo_Location.Bounded_Location.To_Bounded_String("Miami, FL"), 25.7617, -80.1918);
   Location(7) := Geo_Location.Initialize(Geo_Location.Bounded_Location.To_Bounded_String("Nashville, TN"), 36.1745, -86.7680);
   Location(8) := Geo_Location.Initialize(Geo_Location.Bounded_Location.To_Bounded_String("Los Angeles, CA"), 34.1235, -118.274);
   Location(9) := Geo_Location.Initialize(Geo_Location.Bounded_Location.To_Bounded_String("Cincinatti, OH"), 39.1031182, -84.5120196);
   Location(10) := Geo_Location.Initialize(Geo_Location.Bounded_Location.To_Bounded_String("Soule, South Korea"), 37.5665, 126.9780);
   
   for index in 1 .. 10 loop
      Geo_Location.Display_Location(Location(index));
      if index mod 2 = 0 then
         Geo_Location.Display_Distance_And_Bearing(Location(index - 1), Location(index));
         New_Location := Geo_Location.Get_Target_Location(Location(index - 1), 
                                      Geo_Location.Bounded_Location.To_Bounded_String("  New Target"), 
                                      Geo_Location.Get_Distance(Location(index - 1), Location(index),1), -- in miles
--                                        Geo_Location.Get_Azimuth(Location(index - 1), Location(index), 
--                                           Geo_Location.Get_Bearing(Location(index - 1), Location(index))));  -- in degrees
                                      Geo_Location.Get_Bearing(Location(index - 1), Location(index)));  -- in degrees
         New_Location.Display_Location;
         Ada.Text_IO.Put_Line("");
      end if;
   end loop;
   Ada.Text_IO.Put_Line("");   
end Geo_Location_Test;

