with Ada.Strings.Bounded; 
use Ada.Strings.Bounded;

package Geo_Location is
   
   package Bounded_Location is new Generic_Bounded_Length(32);

   type Geo_Location_Type is tagged private;

   function Initialize(Source_Nm : in Bounded_Location.Bounded_String; Source_Lat : in Long_Float; 
                       Source_Lon : in Long_Float) return Geo_Location_Type;
   procedure Get_Location_Name(Location_Nm : out Bounded_Location.Bounded_String);
   procedure Set_Location_Name(Location_Nm : in Bounded_Location.Bounded_String);
   procedure Get_Latitude(Location_Lat : out Long_Float);
   procedure Set_Latitude(Location_Lat : in Long_Float);
   procedure Get_Longitude(Location_Lon : out Long_Float);
   procedure Set_Longitude(Location_Lon : in Long_Float);
   procedure Display_Location(Location_Type : in Geo_Location_Type);
   procedure Display_Distance_And_Bearing(Source_Location, Target_Location : in Geo_Location_Type);
   function Get_Distance(Source : in Geo_Location_Type; Target : in Geo_Location_Type; Units : in Short_Short_Integer) return Long_Float;
   function Get_Bearing(Source : in Geo_Location_Type; Target : in Geo_Location_Type) return Long_Float;
   function Get_Target_Location(Source : in Geo_Location_Type; Target_Nm : Bounded_Location.Bounded_String; 
                       Target_Distance : in Long_Float; Target_Bearing : in Long_Float) return Geo_Location_Type;
   function Get_Azimuth(Source : in Geo_Location_Type; Target : in Geo_Location_Type; Bearing : in Long_Float) return Long_Float;
   function Radians_To_Degrees(radians : Long_Float) return Long_Float;
   
private
   
   RADIUS_IN_MILES : constant Long_Float := 3963.1676; -- Earth radius in miles
   RADIUS_IN_KILOMETERS : constant Long_Float := 6371.0000; -- Earth radius in kilometers

   type Geo_Location_Type is tagged
   record
      Location_Name : Bounded_Location.Bounded_String;
      Latitude : Long_Float;
      Longitude : Long_Float;
      end record;
   
   function Degrees_To_Radians(degrees : Long_Float) return Long_Float;
   function Degrees_To_Radians(degrees, Minutes, Seconds : Long_Float) return Long_Float;
   function Range_In_Kilometers(Source_Latitude : Long_Float; Source_Longitude : Long_Float; 
                         Target_Latitude : Long_Float; Target_Longitude : Long_Float) return Long_Float;
   function Range_In_Miles(Source_Latitude : Long_Float; Source_Longitude : Long_Float;
                         Target_Latitude : Long_Float; Target_Longitude : Long_Float) return Long_Float;
   function Arctan2(yCoord : Long_Float; xCoord : Long_Float) return Long_Float;
   
end Geo_Location;
