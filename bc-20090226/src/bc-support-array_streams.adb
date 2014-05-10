--  Copyright 2003 Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $Revision: 1400 $
--  $Date: 2009-02-21 21:39:12 +0000 (Sat, 21 Feb 2009) $
--  $Author: simonjwright $

with Ada.IO_Exceptions;

package body BC.Support.Array_Streams is


   use type Ada.Streams.Stream_Element_Offset;


   function Last (Used_In : Stream_Type)
                 return Ada.Streams.Stream_Element_Offset is
   begin
      return Used_In.Next_Write - 1;
   end Last;


   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
      Available : constant Ada.Streams.Stream_Element_Offset
        := Stream.Next_Write - Stream.Next_Read;
      Required : constant Ada.Streams.Stream_Element_Offset
        := Item'Last + 1 - Item'First;
   begin
      if Required < Available then
         Item := Stream.Buffer (Stream.Next_Read
                                  .. Stream.Next_Read + Required - 1);
         Stream.Next_Read := Stream.Next_Read + Required;
         Last := Item'Last;
      else
         Item (Item'First .. Item'First + Available - 1)
           := Stream.Buffer (Stream.Next_Read .. Stream.Next_Write - 1);
         Stream.Next_Read := Stream.Next_Write;
         Last := Item'First + Available - 1;
      end if;
   end Read;


   procedure Reset (Stream : out Stream_Type) is
   begin
      Stream.Next_Write := Stream.Buffer'First;
      Stream.Next_Read := Stream.Buffer'First;
   end Reset;


   procedure Set_Last (Used_In : in out Stream_Type;
                       To : Ada.Streams.Stream_Element_Offset) is
   begin
      Used_In.Next_Write := To + 1;
   end Set_Last;


   procedure Write
     (Stream : in out Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array) is
      Length : constant Ada.Streams.Stream_Element_Offset
        := Item'Last + 1 - Item'First;
   begin
      if Stream.Next_Write + Length > Stream.Buffer'Last + 1 then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      Stream.Buffer (Stream.Next_Write .. Stream.Next_Write + Length - 1)
        := Item;
      Stream.Next_Write := Stream.Next_Write + Length;
   end Write;


end BC.Support.Array_Streams;
