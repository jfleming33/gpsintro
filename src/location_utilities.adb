with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;
use Ada.Text_IO;

package body Location_Utilities is

   function Get_Azimuth2(Source_Latitude, Source_Longitude,
                         Target_Latitude, Target_Longitude : Float) return Float is

      cPI : constant Float := Ada.Numerics.Pi; --3.1415926536;
      cNO_ANGLE : constant Float := -999.0;
      result, result1, result2 : Float;
      deltaX, deltaY : Float;

   begin
      deltaX := Target_Latitude - Source_Latitude;
      deltaY := Target_Longitude - Source_Longitude;

      if (deltaX > 0.0) then  result := (cPI * 0.5) - ArcTan(deltaY / deltaX);
      elsif (deltaX < 0.0) then  result := (cPI * 1.5) - ArcTan(deltaY / deltaX);
      elsif (deltaY > 0.0) then  result := 0.0;
      elsif (deltaY < 0.0) then  result := cPI;
      else  result := cNO_ANGLE;   -- the 2 points are equal
      end if;
      Put_Line("");
      Put_Line("Radians: " & Float'Image(result));
      result1 := (result * 180.0) / cPI;
      Put_Line("Azimuth1: " & Float'Image(result1));
      result2 := (180.0 / cPI) * result;
      Put_Line("Azimuth2: " & Float'Image(result2));
      return result;
   end Get_Azimuth2;


end Location_Utilities;
