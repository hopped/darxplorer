-- =================================================================
-- Calendar_Formatter.Ads
-- by Richard Riehle, AdaWorks Software Engineering 
-- http://www.adaworks.com
-- This package allows a client to create time in a variety of
-- formats. It is based on Ada.Calendar as defined in Chapter 9
-- of the Ada Language Reference Manual.  You may first get the 
-- Time using one of the two  Get functions. Then get the formatted 
-- time by calling one of the formatted time functions.
-- This package is not warranted by the author.  There are no
-- guarantees either implied or specified.  It is available for
-- experimentation and student use.  If you plan to use it in any
-- production quality software, you should test it thoroughly and
-- make corrections as you require.
--                 Richard Riehle
-- NOTE: Not all functions are implemented in this version. Future
-- versions will contain full implementation along with more features
-- such as child packages for doing comparisons and calculations.
-- =================================================================
package Calendar_Formatter is

   type Formatted_Time is private;  -- cannot be limited in this design because
                                    -- one function returns object of the type and
                                    -- requires an assignment statement
   type Time_Reference is access all Formatted_Time;
   type Month_1 is (January,   February, March,    April,
                    May,       June,     July,     August,
                    September, October,  November, December);
   type Month_2 is (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);

   type Day_1   is (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
   type Day_2   is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   subtype Work_Day_1 is Day_1 range Monday..Friday;
   subtype Work_Day_2 is Day_2 range Mon..Fri;
   function Get_Time return Formatted_Time;
   function Get_Time return Time_Reference;
   function YY   (FT : Formatted_Time)    return String;      -- two character string for year
   function YYYY (FT : Formatted_Time)    return String;      -- four character string for year
   function DD   (FT : Formatted_Time)    return String;      -- two character string for day
   function MM   (FT : Formatted_Time)    return String;      -- two character string for month
   function Hour12 (FT : Formatted_Time)  return String;      -- two character string 12 hour clock hour
   function AMPM   (FT : Formatted_Time)  return String;      -- two character string;  AM or PM
   function Hour24 (FT : Formatted_Time)  return String;      -- two character string 24 hour clock hour
   function Minute (FT : Formatted_Time)  return String;      -- two character string minute
   function Month  (FT : Formatted_Time)  return String;      -- Month fully spelled out
   function Month  (FT : Formatted_Time)  return Month_1;     -- Month in format of the Type;
--   function Month (FT : Formatted_Time)   return Month_2;     -- Not yet implemented
--   function Day   (FT : Formatted_Time)   return Day_1;       -- Not yet implemented
--   function Day   (FT : Formatted_Time)   return Day_2;       -- Not yet implemented
   function Format_1 (FT : Formatted_Time) return Formatted_Time;  -- Returns a copy of the private type
   function Format_2 (FT : Formatted_Time) return String;      -- formatted MM-DD-YY
   function Format_3 (FT : Formatted_Time) return String;      -- formatted MM-DD-YYYY HH:MM:SS
   function Format_4 (FT : Formatted_Time) return String;      -- e.g., July 21, 1999
   function Total_Seconds (FT : Formatted_Time) return String; -- Raw number of seconds in Time

   Time_Format_Error : exception;
private

   type Formatted_Time is
     record
        Year       : String(1..4) := (others => ' ');
        Month      : String(1..2) := (others => ' ');
        Day        : String(1..2) := (others => ' ');
        Hour24     : String(1..2) := (others => ' ');
        Hour12     : String(1..2) := (others => ' ');
        Minute     : String(1..2) := (others => ' ');
        Seconds    : String(1..2) := (others => ' ');
        AMPM       : String(1..2) := (others => ' ');
        Noon_Mid   : String(1..8) := "Midnight";   -- or "Noon    ";
        Month_Name : Month_1;
        Day_Name   : Day_1;
        Holiday    : String(1..15) := (others => ' ');
     end record;

end Calendar_Formatter;