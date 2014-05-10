with Ada.Calendar;         -- ALRM  9.6/10 
with Ada.Integer_Text_IO;  -- ALRM  A.10.8
package body Calendar_Formatter is
   use Ada;                -- Make package Ada directly visible
   use Calendar;           -- Make package Calendar directly visible
   package IIO renames Ada.Integer_Text_IO;  -- for dot notation
   -- Do a simple function call on Ada.Calendar.Clock;
   Package_Level_Formatted_Time : Formatted_Time;

   procedure HourMin_Formatter (The_Formatted_Time : in out Formatted_Time;
                                The_Time           : in     Calendar.Time 
) is
     The_Duration : Calendar.Day_Duration := Calendar.Seconds(The_Time);
     Hour    : Integer := 0;
     Hour12  : Integer := 0;
     Hour24  : Integer := 0;
     Minute  : Integer := 0;
     Seconds : Integer := 0;
     The_Seconds : Integer := Integer(The_Duration);
     Remainder : Integer := 0;
     AMPM : String (1..2) := "PM";
   begin
      Hour      := The_Seconds / 3600;
      Hour24    := Hour;
      Remainder := The_Seconds mod 3600;
      Minute    := Remainder / 60;
      Seconds   := Remainder mod 60;
      if Hour > 12 then
         if Hour = 24 then
            AMPM := "AM";
            Hour12 := 12;
            The_Formatted_Time.Noon_Mid := "Midnight";
         else
            Hour12 := Hour - 12;
            AMPM := "PM";
         end if;
      else
         if Hour = 12 then
            Hour12 := 12;
            AMPM := "PM";
            The_Formatted_Time.Noon_Mid := "Noon    ";
         else
            AMPM := "AM";
         end if;
      end if;
      IIO.Put(To => The_Formatted_Time.Hour12,    Item => Hour12);
      IIO.Put(To => The_Formatted_Time.Hour24,    Item => Hour24);
      IIO.Put(To => The_Formatted_Time.Minute,    Item => Minute);
      IIO.Put(To => The_Formatted_Time.Seconds,   Item => Seconds);

      The_Formatted_Time.AMPM := AMPM;
      Package_Level_Formatted_Time := The_Formatted_Time;
--      if Hour24 = 0 then -- or else Minute = 0 or else Seconds = 0 then
--         raise Time_Format_Error;
--      end if;
   end HourMin_Formatter;

   procedure Day_of_Week_Formatter (The_Formatted_Time : in out 
Formatted_Time) is
   begin
      null;
      Package_Level_Formatted_Time := The_Formatted_Time;
   end Day_of_Week_Formatter;

   procedure Month_Day_Year_Formatter (The_Formatted_Time : in out Formatted_Time;
                                       The_Time           : in     Calendar.Time) is
       Day   : Calendar.Day_Number   := Calendar.Day(The_Time);
       Month : Calendar.Month_Number := Calendar.Month(The_Time);
       Year  : Calendar.Year_Number  := Calendar.Year(The_Time);
       Position : Integer := Month - 1;
   begin
       IIO.Put(To => The_Formatted_Time.Day,     Item => Day);
       IIO.Put(To => The_Formatted_Time.Month,   Item => Month);
       IIO.Put(To => The_Formatted_Time.Year,    Item => Year);
       The_Formatted_Time.Month_Name := Month_1'Val(Position);
   end Month_Day_Year_Formatter;

   procedure Main_Formatter (F : in out Formatted_Time;
                             T : in     Calendar.Time) is
   begin
      Month_Day_Year_Formatter(F, T);
      HourMin_Formatter(F, T);
      Day_Of_Week_Formatter(F);
      Package_Level_Formatted_Time := F;
   end Main_Formatter;

   function Get_Time return Formatted_Time is  -- return string of time
     The_Formatted_Time : Formatted_Time;
     The_Time : Calendar.Time := Calendar.Clock;
   begin
     Main_Formatter(The_Formatted_Time, The_Time);
     return The_Formatted_Time;
   end Get_Time;

   function Get_Time return Time_Reference is
      Time   : Formatted_Time := Get_Time;
      Result : Time_Reference := new Formatted_Time'(Time);
   begin
      return Result;
   end Get_Time;

   function YY (FT : Formatted_Time)      return String is      -- two character string for year
   begin
      return FT.Year(3..4);
   end YY;

   function YYYY  (FT : Formatted_Time)   return String is     -- four character string for year
   begin
      return FT.Year;
   end YYYY;

   function DD  (FT : Formatted_Time)     return String is     -- two character string for day
   begin
      return FT.Day;
   end DD;

   function MM  (FT : Formatted_Time)     return String is     -- two character string for month
   begin
     return FT.Month;
   end MM;

   function Hour12 (FT : Formatted_Time)  return String is     -- two character string 12 hour clock hour
   begin
      return FT.Hour12;
   end Hour12;

   function AMPM (FT : Formatted_Time)    return String is     -- two character string;  AM or PM
   begin
     return FT.AMPM;
   end AMPM;

   function Hour24 (FT : Formatted_Time)  return String is     -- two character string 24 hour clock hour
   begin
      return FT.Hour24;
   end Hour24;

   function Minute (FT : Formatted_Time)  return String is     -- two character string minute
   begin
      return FT.Minute;
   end Minute;
-- ======================

   function Month (FT : Formatted_Time)   return String is      -- Month fully spelled out
      Position : Integer := Month_1'Pos(FT.Month_Name);
   begin
      return Month_1'Image(Month_1'Val(Position));
   end Month;

   function Month (FT : Formatted_Time)   return Month_1 is
   begin
      return FT.Month_Name;
   end Month;

   function Month (FT : Formatted_Time) return Month_2 is
      Position : Integer := Month_1'Pos(FT.Month_Name);
   begin
      return Month_2'Val(Position);
   end Month;

   function Day  (FT : Formatted_Time)    return Day_1 is
   begin
      return FT.Day_Name;
   end Day;

   function Day (FT : Formatted_Time)     return Day_2 is
     Position : Integer := Day_1'Pos(FT.Day_Name);
   begin
      return Day_2'Val(Position);
   end Day;

   function Format_1 (FT : Formatted_Time) return Formatted_Time is
   begin
      return FT;
   end Format_1;

   function Format_2 (FT : Formatted_Time) return String is      -- formatted MM-DD-YY
      Result : String(1..8) := (others => ' ');
   begin
       Result := (FT.Month   & "-" & FT.Day    & "-" & FT.Year);
       return Result;
   end Format_2;

   function Format_3 (FT : Formatted_Time)return String is      -- formatted MM-DD-YYYY HH:MM:SS
     Result : String := (FT.Month   & "-" & FT.Day    & "-" & FT.Year & " " &
                         FT.Hour24  & ":" & FT.Minute & ":" & FT.Seconds);
   begin
      return Result;
   end Format_3;

   function Format_4 (FT : Formatted_Time) return String is      -- formatted Month-Name DD YYYY HH:MM:SS
     Result : String := (Month_1'Image(FT.Month_Name)
                         & " " & FT.Day    & ", " & FT.Year & " " &
                         FT.Hour24  & ":" & FT.Minute & ":" & FT.Seconds);
   begin
      return Result;
   exception
     when Constraint_Error =>
       return ("Constraint Error in Format_4 function ");
   end Format_4;


   function Total_Seconds (FT : Formatted_Time) return String is
      The_Time : Calendar.Time := Clock;
      The_Seconds : Calendar.Day_Duration := Seconds(The_Time);
      Converted   : Integer := Integer(The_Seconds);
   begin
      return Calendar.Day_Duration'Image(The_Seconds);
   end Total_Seconds;
end Calendar_Formatter;