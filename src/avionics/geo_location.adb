pragma Ada_2012;

with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Text_IO.Bounded_IO;
with Ada.Strings.Bounded;
use Ada.Strings.Bounded;

package body Geo_Location is

   this : Geo_Location_Type;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Source_Nm : in Bounded_Location.Bounded_String;
      Source_Lat : in Long_Float;
      Source_Lon : in Long_Float) return Geo_Location_Type
   is
      Location_Type : Geo_Location.Geo_Location_Type;
   begin
      --Location_Type := new Geo_Location.Geo_Location_Type;
      Location_Type.Location_Name := Source_Nm;
      Location_Type.Latitude := Source_Lat;
      Location_Type.Longitude := Source_Lon;
      return Location_Type;
   end Initialize;

   -----------------------
   -- Get_Location_Name --
   -----------------------

   procedure Get_Location_Name (Location_Nm : out Bounded_Location.Bounded_String) is
   begin
      Location_Nm := this.Location_Name;
   end Get_Location_Name;

   -----------------------
   -- Set_Location_Name --
   -----------------------

   procedure Set_Location_Name (Location_Nm : in Bounded_Location.Bounded_String) is
   begin
      this.Location_Name := Location_Nm;
   end Set_Location_Name;

   ------------------
   -- Get_Latitude --
   ------------------

   procedure Get_Latitude (Location_Lat : out Long_Float) is
   begin
      Location_Lat := this.Latitude;
   end Get_Latitude;

   ------------------
   -- Set_Latitude --
   ------------------

   procedure Set_Latitude (Location_Lat : in Long_Float) is
   begin
      this.Latitude := Location_Lat;
   end Set_Latitude;

   -------------------
   -- Get_Longitude --
   -------------------

   procedure Get_Longitude (Location_Lon : out Long_Float) is
   begin
      Location_Lon := this.Longitude;
   end Get_Longitude;

   -------------------
   -- Set_Longitude --
   -------------------

   procedure Set_Longitude (Location_Lon : in Long_Float) is
   begin
      this.Longitude := Location_Lon;
   end Set_Longitude;

   ----------------------------------
   -- Display_Distance_And_Bearing --
   ----------------------------------

   procedure Display_Distance_And_Bearing(Source_Location, Target_Location : in Geo_Location_Type) is
      kilometers, miles, bearing, azimuth : Long_Float;
   begin
      kilometers := Get_Distance(Source_Location, Target_Location, 0);
      miles := Get_Distance(Source_Location, Target_Location, 1);
      bearing := Get_Bearing(Source_Location, Target_Location);
      azimuth := Get_Azimuth(Source_Location, Target_Location, bearing);

      Ada.Text_IO.Put("  Range = ");
      Ada.Long_Float_Text_IO.Put(kilometers, 3, 7, 0);
      Ada.Text_IO.Put(" kilometers (");
      Ada.Long_Float_Text_IO.Put(miles, 3, 7, 0);
      Ada.Text_IO.Put(" miles), Bearing = ");
      Ada.Long_Float_Text_IO.Put(bearing, 3, 7, 0);
      Ada.Text_IO.Put(" degrees, Azimuth = ");
      Ada.Long_Float_Text_IO.Put(azimuth, 3, 7, 0);
      Ada.Text_IO.Put_Line("");
   end Display_Distance_And_Bearing;

   ----------------------
   -- Display_Location --
   ----------------------

   procedure Display_Location(Location_Type : in Geo_Location_Type) is
   begin
      Ada.Text_IO.Put(Bounded_Location.To_String(Location_Type.Location_Name));
      Ada.Text_IO.Put(" (");
      Ada.Long_Float_Text_IO.Put(Location_Type.Latitude, 3, 7, 0);
      Ada.Text_IO.Put(", ");
      Ada.Long_Float_Text_IO.Put(Location_Type.Longitude, 3, 7, 0);
      Ada.Text_IO.Put_Line(")");
   end Display_Location;

   ------------------
   -- Get_Distance --
   ------------------

   function Get_Distance
     (Source : in Geo_Location_Type;
      Target : in Geo_Location_Type;
      Units  : in Short_Short_Integer)
      return Long_Float
   is
      Distance : Long_Float;
   begin
      if Units = 0 then
         Distance := Range_In_Kilometers(Source.Latitude, Source.Longitude,
                                         Target.Latitude, Target.Longitude);
      else
         Distance := Range_In_Miles(Source.Latitude, Source.Longitude,
                                         Target.Latitude, Target.Longitude);
      end if;
      return Distance;
   end Get_Distance;

   -----------------
   -- Get_Azimuth --
   -----------------

   function Get_Azimuth
     (Source : in Geo_Location_Type;
      Target : in Geo_Location_Type;
      Bearing : in Long_Float) -- in degrees
      return Long_Float
   is
      Latitude1 : Long_Float;
      Latitude2 : Long_Float;
      Latitude_Delta, Longitude_Delta : Long_Float;
      xCoord, yCoord : Long_Float;
      azimuth : Long_Float;
   begin
      -- first, let's get our inputs in radians
      Latitude1 := Degrees_To_Radians(Source.Latitude);
      Latitude2 := Degrees_To_Radians(Target.Latitude);
      Longitude_Delta := Degrees_To_Radians(Target.Longitude - Source.Longitude);

      yCoord := Sin(Longitude_Delta) * Cos(Latitude2);
      xCoord := Cos(Latitude1) * Sin(Latitude2) - Sin(Latitude1) * Cos(Latitude2) * Cos(Longitude_Delta);

      if xCoord >= 0.0 and yCoord >= 0.0 then -- Q1
         azimuth := bearing;
      elsif xCoord < 0.0 and yCoord >= 0.0 then -- Q2
         azimuth := Abs(180.0 - bearing);
      elsif xCoord < 0.0 and yCoord < 0.0 then -- Q3
         azimuth := 180.0 + Abs(bearing);
      elsif xCoord >= 0.0 and yCoord < 0.0 then -- Q4
         azimuth := 360.0 - Abs(bearing);
      else
         azimuth := -999.9; -- undefined
      end if;

      return azimuth;

   end Get_Azimuth;

   -----------------
   -- Get_Bearing --
   -----------------

   function Get_Bearing
     (Source : in Geo_Location_Type;
      Target : in Geo_Location_Type)
      return Long_Float -- in degrees
   is
      Latitude1 : Long_Float;
      Latitude2 : Long_Float;
      Latitude_Delta, Longitude_Delta : Long_Float;
      x, y, z : Long_Float;
      i : Long_Integer;
   begin
      -- first, let's get our inputs in radians
      Latitude1 := Degrees_To_Radians(Source.Latitude);
      Latitude2 := Degrees_To_Radians(Target.Latitude);
      --Latitude_Delta := Degrees_To_Radians(Target.Latitude - Source.Latitude);
      Longitude_Delta := Degrees_To_Radians(Target.Longitude - Source.Longitude);

--        Ada.Text_IO.Put("Source Latitude = ");
--        Ada.Long_Float_Text_IO.Put(Source.Latitude, 3, 8, 0);
--        Ada.Text_IO.Put(" degrees (");
--        Ada.Long_Float_Text_IO.Put(Latitude1, 3, 8, 0);
--        Ada.Text_IO.Put(" radians)");
--        Ada.Text_IO.Put(", Source Longitude = ");
--        Ada.Long_Float_Text_IO.Put(Source.Longitude, 3, 8, 0);
--        Ada.Text_IO.Put(" degrees (");
--        Ada.Long_Float_Text_IO.Put(Degrees_To_Radians(Source.Longitude), 3, 8, 0);
--        Ada.Text_IO.Put_Line(" radians)");
--
--        Ada.Text_IO.Put("Target Latitude = ");
--        Ada.Long_Float_Text_IO.Put(Target.Latitude, 3, 8, 0);
--        Ada.Text_IO.Put(" degrees (");
--        Ada.Long_Float_Text_IO.Put(Latitude2, 3, 8, 0);
--        Ada.Text_IO.Put(" radians)");
--        Ada.Text_IO.Put(", Target Longitude = ");
--        Ada.Long_Float_Text_IO.Put(Target.Longitude, 3, 8, 0);
--        Ada.Text_IO.Put(" degrees (");
--        Ada.Long_Float_Text_IO.Put(Degrees_To_Radians(Target.Longitude), 3, 8, 0);
--        Ada.Text_IO.Put_Line(" radians)");
--
--        Ada.Text_IO.Put("Longitude_Delta = ");
--        Ada.Long_Float_Text_IO.Put(Target.Longitude - Source.Longitude, 3, 8, 0);
--        Ada.Text_IO.Put(" degrees (");
--        Ada.Long_Float_Text_IO.Put(Longitude_Delta, 3, 8, 0);
--        Ada.Text_IO.Put_Line(" radians)");

      y := Sin(Longitude_Delta) * Cos(Latitude2);
      x := Cos(Latitude1) * Sin(Latitude2) - Sin(Latitude1) * Cos(Latitude2) * Cos(Longitude_Delta);

      z := Arctan2(y, x);
      --z := Arctan2(Longitude_Delta, Latitude_Delta);

--        Ada.Text_IO.Put("zRadians = ");
--        Ada.Long_Float_Text_IO.Put(z, 3, 8, 0);
--        Ada.Text_IO.Put_Line("");

      --z := Radians_To_Degrees(z) + 360.0;
      z := Radians_To_Degrees(z);
--        Ada.Text_IO.Put("zDegrees = ");
--        Ada.Long_Float_Text_IO.Put(z, 3, 8, 0);
--        Ada.Text_IO.Put_Line("");

--        i := Long_Integer(z * 1000000.0);
--        i := (i mod 360000000);
--
--        z := Long_Float(i) ;
--        z := z / 1000000.0;
--
--        Ada.Text_IO.Put("z3 = ");
--        Ada.Long_Float_Text_IO.Put(z, 3, 8, 0);
--        Ada.Text_IO.Put_Line("");

      return z;
   end Get_Bearing;

   -------------------------
   -- Get_Target_Location --
   -------------------------

   function Get_Target_Location
     (Source : Geo_Location_Type;
      Target_Nm : Bounded_Location.Bounded_String;
      Target_Distance : in Long_Float; -- in miles
      Target_Bearing : in Long_Float)  -- in degrees
      return Geo_Location_Type
   is
      Target : Geo_Location_Type;
      brngRad : Long_Float;
      srcLatRad, srcLonRad : Long_Float;
      tarLatRad, tarLonRad : Long_Float;
      earthRadius : Long_Float;
      distFrac : Long_Float;
      radNum, radDen : Long_Float;
--        i, j, k : Long_Integer;
--        a : Long_Float;
   begin
      Target := Initialize(Target_Nm, 0.0, 0.0);

      brngRad := Degrees_To_Radians(Target_Bearing);
      srcLatRad :=  Degrees_To_Radians(Source.Latitude);
      srcLonRad :=  Degrees_To_Radians(Source.Longitude);
      earthRadius := RADIUS_IN_MILES;
      distFrac := Target_Distance / earthRadius;

      tarLatRad := Arcsin(Sin(srcLatRad) * Cos(distFrac) +
                            Cos(srcLatRad) * Sin(distFrac) * Cos(brngRad));
      radNum := Sin(brngRad) * Sin(distFrac) * Cos(srcLatRad);
      radDen := Cos(distFrac) - Sin(srcLatRad) * Sin(tarLatRad);

        Ada.Text_IO.Put("  >> radNumR = ");
        Ada.Long_Float_Text_IO.Put(radNum, 3, 8, 0);
        Ada.Text_IO.Put(", radDenR = ");
        Ada.Long_Float_Text_IO.Put(radDen, 3, 8, 0);
        Ada.Text_IO.Put_Line("");

        Ada.Text_IO.Put("  >> cosDis =");
        Ada.Long_Float_Text_IO.Put(Cos(distFrac), 3, 8, 0);
        Ada.Text_IO.Put(" - (sinSrcLatR =");
        Ada.Long_Float_Text_IO.Put(Sin(srcLatRad), 3, 8, 0);
        Ada.Text_IO.Put(" * sinTarLatR =");
        Ada.Long_Float_Text_IO.Put(Sin(tarLatRad), 3, 8, 0);
        Ada.Text_IO.Put_Line(")");

      tarLonRad := srcLonRad + Arctan2(Sin(brngRad) * Sin(distFrac) * Cos(srcLatRad),
                   Cos(distFrac) - Sin(srcLatRad) * Sin(tarLatRad));

        Ada.Text_IO.Put("  >> tLatR = ");
        Ada.Long_Float_Text_IO.Put(tarLatRad, 3, 8, 0);
        Ada.Text_IO.Put(", tLonR = ");
        Ada.Long_Float_Text_IO.Put(tarLonRad, 3, 8, 0);
        Ada.Text_IO.Put_Line("");

      Target.Latitude := Radians_To_Degrees(tarLatRad);
      Target.Longitude := Radians_To_Degrees(tarLonRad);

      --Target.Longitude := Radians_To_Degrees((lonRad + a + 3.0 * Ada.Numerics.Pi) mod (2.0 * Ada.Numerics.Pi) - Ada.Numerics.Pi);
        Ada.Text_IO.Put("  >> tLatD = ");
        Ada.Long_Float_Text_IO.Put(Target.Latitude, 3, 8, 0);
        Ada.Text_IO.Put(", tLonD = ");
        Ada.Long_Float_Text_IO.Put(Target.Longitude, 3, 8, 0);
        Ada.Text_IO.Put_Line("");

--        i := Long_Integer((lonRad + a + 3.0 * Ada.Numerics.Pi) * 1000000.0);
--        j := Long_Integer((2.0 * Ada.Numerics.Pi) * 1000000.0);
--        k := i mod j;
--
--        a := Long_Float(k) ;
--        a := a / 1000000.0;

--          a := lonRad + a;
--
--        Target.Longitude := Radians_To_Degrees(a);

      return Target;
end Get_Target_Location;

--      public GeoLocation getTargetLocation( String targetName, double distance, double bearing) {
--          GeoLocation target = new GeoLocation(targetName, 0.0, 0.0);
--
--          double brngRad = Math.toRadians(bearing);
--          double latRad = Math.toRadians(this.latitude);
--          double lonRad = Math.toRadians(this.longitude);
--          double earthRadius = RADIUS_IN_MILES;
--          double distFrac = distance / earthRadius;
--
--          target.setLatitude(Math.toDegrees(Math.asin(Math.sin(latRad) *
--                 Math.cos(distFrac) + Math.cos(latRad) * Math.sin(distFrac) * Math.cos(brngRad))));
--          double a = Math.atan2(Math.sin(brngRad) * Math.sin(distFrac) * Math.cos(latRad),
--                 Math.cos(distFrac) - Math.sin(latRad) * Math.sin(target.getLatitude()));
--          target.setLongitude(Math.toDegrees((lonRad + a + 3 * Math.PI) % (2 * Math.PI) - Math.PI));
--
--          return target;
--      }

   ------------------------
   -- Radians_To_Degrees --
   ------------------------

   function Radians_To_Degrees(radians : Long_Float) return Long_Float is
      degrees : Long_Float;
   begin
      --radians := degrees * (Ada.Numerics.Pi / 180.0);
      --degrees := (Ada.Numerics.Pi / 180.0) / radians;
      degrees := radians * (180.0 / Ada.Numerics.Pi);
      return degrees;
   end Radians_To_Degrees;

   ------------------------
   -- Degrees_To_Radians --
   ------------------------

   function Degrees_To_Radians(degrees : Long_Float) return Long_Float is
      radians : Long_Float;
   begin
      radians := degrees * (Ada.Numerics.Pi / 180.0);
      return radians;
   end Degrees_To_Radians;

   ------------------------
   -- Degrees_To_Radians --
   ------------------------

   function Degrees_To_Radians(degrees, Minutes, Seconds : Long_Float) return Long_Float is
      radians : Long_Float;
   begin
      radians := (degrees + (Minutes / 60.0) + (Seconds /3600.0)) * (Ada.Numerics.Pi / 180.0);
      return radians;
   end Degrees_To_Radians;

   -------------------------
   -- Range_In_Kilometers --
   -------------------------

   function Range_In_Kilometers(Source_Latitude : Long_Float; Source_Longitude : Long_Float;
                         Target_Latitude : Long_Float; Target_Longitude : Long_Float) return Long_Float is
      Earth_Radius : constant := RADIUS_IN_KILOMETERS; -- in kilometers
      a : Long_Float := Sin ((Degrees_To_Radians(Target_Latitude) -
                          Degrees_To_Radians(Source_Latitude)) / 2.0);
      b : Long_Float := Sin ((Degrees_To_Radians(Target_Longitude) -
                          Degrees_To_Radians(Source_Longitude)) / 2.0);
   begin
      return 2.0 * Earth_Radius * ArcSin (Sqrt (a * a + Cos (Degrees_To_Radians(Source_Latitude)) *
                                            Cos (Degrees_To_Radians(Target_Latitude)) * b * b));
   end Range_In_Kilometers;

   --------------------
   -- Range_In_Miles --
   --------------------

   function Range_In_Miles(Source_Latitude : Long_Float; Source_Longitude : Long_Float;
                         Target_Latitude : Long_Float; Target_Longitude : Long_Float) return Long_Float is
      Earth_Radius : constant := RADIUS_IN_MILES; -- in miles
      a : Long_Float := Sin ((Degrees_To_Radians(Target_Latitude) -
                          Degrees_To_Radians(Source_Latitude)) / 2.0);
      b : Long_Float := Sin ((Degrees_To_Radians(Target_Longitude) -
                          Degrees_To_Radians(Source_Longitude)) / 2.0);
   begin
      return 2.0 * Earth_Radius * ArcSin (Sqrt (a * a + Cos (Degrees_To_Radians(Source_Latitude)) *
                                            Cos (Degrees_To_Radians(Target_Latitude)) * b * b));
   end Range_In_Miles;

   -------------
   -- Arctan2 --
   -------------

   function Arctan2(yCoord : Long_Float; xCoord : Long_Float) return Long_Float is
      atan2 : Long_Float;
   begin
--        Ada.Text_IO.Put("x = ");
--        Ada.Long_Float_Text_IO.Put(xCoord, 3,8,0);
--        Ada.Text_IO.Put(", y = ");
--        Ada.Long_Float_Text_IO.Put(yCoord, 3,8,0);
--        Ada.Text_IO.Put_Line("");

      if xCoord > 0.0 then  -- Q1 and Q4
         atan2 := Arctan(yCoord, xCoord);
         Ada.Text_IO.Put_Line("Q1/Q4");
      elsif xCoord < 0.0 and yCoord >= 0.0 then  -- Q2
         atan2 := Arctan(yCoord, xCoord) + Ada.Numerics.Pi;
         Ada.Text_IO.Put_Line("Q2");
      elsif xCoord < 0.0 and yCoord < 0.0 then  -- Q3
         atan2 := Arctan(yCoord, xCoord) - Ada.Numerics.Pi;
         Ada.Text_IO.Put_Line("Q3");
      elsif xCoord = 0.0 and yCoord > 0.0 then
         atan2 := Ada.Numerics.Pi / 2.0;
      elsif xCoord = 0.0 and yCoord < 0.0 then
         atan2 := (Ada.Numerics.Pi / 2.0) * (-1.0);
      else
         atan2 := 0.0; -- undefined
      end if;

      return atan2;
   end Arctan2;


end Geo_Location;

