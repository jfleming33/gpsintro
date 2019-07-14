with Message_For_User;
with Location_Utilities;
with Geo_Location;
with Ada.Strings.Bounded;

procedure Main is
  azimuth : Float;
  type Location is array (1 .. 10) of Geo_Location.Geo_Location_Type;
begin
   Message_For_User.Print_Hello_World;
   Message_For_User.Display_Time;
   -- 32.3668° N, 86.3000° W Montgomery
   -- 32.6130° N, 83.6242° W Warner Robins
   azimuth := Location_Utilities.Get_Azimuth2(32.3668, -86.3000, 32.6130, -83.6242);


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
      end if;
   end loop;
   Ada.Text_IO.Put_Line("");

end Main;
