--  Copyright 1994 Grady Booch
--  Copyright 2003 Martin Krischik
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2002 Simon Wright <simon@pushface.org>

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
pragma License (Modified_GPL);
pragma Ada_05;

--  $Revision: 1391 $
--  $Date: 2009-01-12 20:55:33 +0000 (Mon, 12 Jan 2009) $
--  $Author: simonjwright $

generic
package BC.Indefinite_Containers.Indefinite_Iterators is

   pragma Elaborate_Body;

   generic
      --  Call Apply with a Parameter for each Item in the Container to
      --  which the iterator Using is bound. The iteration will terminate
      --  early if Apply sets OK to False.
      type
         Param_Type (<>)
      is limited private;

      with procedure Apply (
         Elem : in Item;
         Param : in Param_Type;
         OK : out Boolean);

   procedure Visit_With_In_Param (
      Using : in out Iterator'Class;
      Param : in Param_Type);

   generic
      --  Call Apply with a Parameter for each Item in the Container to
      --  which the iterator Using is bound. The iteration will terminate
      --  early if Apply sets OK to False.
      type
         Param_Type (<>)
      is limited private;

      with procedure Apply (
         Elem : in Item;
         Param : in out Param_Type;
         OK : out Boolean);

   procedure Visit_With_In_Out_Param (
      Using : in out Iterator'Class;
      Param : in out Param_Type);

   generic
      --  Call Apply with a Parameter each Item in the Container to which
      --  the iterator Using is bound. The iteration will terminate early
      --  if Apply sets OK to False.
      type
         Param_Type (<>)
      is limited private;

      with procedure Apply (
         Elem : in out Item;
         Param : in Param_Type;
         OK : out Boolean);

   procedure Modify_With_In_Param (
      Using : in out Iterator'Class;
      Param : in Param_Type);

   generic
      --  Call Apply with a copy of each Item in the Container to which
      --  the iterator Using is bound. The iteration will terminate early
      --  if Apply sets OK to False.
      type
         Param_Type (<>)
      is limited private;

      with procedure Apply (
         Elem : in out Item;
         Param : in out Param_Type;
         OK : out Boolean);

   procedure Modify_With_In_Out_Param (
      Using : in out Iterator'Class;
      Param : in out Param_Type);

private

end BC.Indefinite_Containers.Indefinite_Iterators;
