with Ada.Text_IO;
with Ada.Calendar;
with Gnat.Calendar.Time_IO;

package body Message_For_User is

   -----------------------
   -- Print_Hello_World --
   -----------------------

   procedure Print_Hello_World is

   begin
      Ada.Text_IO.Put_Line("Hello Ada world!");
   end Print_Hello_World;


   procedure Display_Time is

   begin
      Gnat.Calendar.Time_IO.Put_Time(Ada.Calendar.Clock, "Date: %Y/%m/%d Time: %H:%M:%S");
   end Display_Time;

end Message_For_User;
