--  Copyright 1994 Grady Booch
--  Copyright 2003 Martin Krischik
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

with BC.Support.Indefinite_Unbounded;
pragma Elaborate_All (BC.Support.Indefinite_Unbounded);
with System.Storage_Pools;

generic
  Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;
package BC.Indefinite_Containers.Collections.Ordered.Unbounded is

  pragma Elaborate_Body;

  type Collection is new Abstract_Ordered_Collection with private;

  function Null_Container return Collection;

  function "=" (Left, Right : in Collection) return Boolean;

  procedure Clear (C : in out Collection);
  --  Empty the collection of all items.

  procedure Insert (C : in out Collection; Elem : Item);
  --  Add the item to the collection, starting at the front.

  procedure Insert (C : in out Collection;
               Elem : Item;
               Before : Positive);
  --  Add the item to the collection, starting at the front.

  procedure Append (C : in out Collection; Elem : Item);
  --  Add the item to the collection, starting at the end.

  procedure Append (C : in out Collection;
               Elem : Item;
               After : Positive);
  --  Add the item to the collection, starting at the end.

  procedure Remove (C : in out Collection; At_Index : Positive);
  --  Remove the item at the given index in the collection.

  procedure Replace (C : in out Collection;
                At_Index : Positive;
                Elem : Item);
  --  Replace the item at the given index with the given item.

  function Length (C : Collection) return Natural;
  --  Return the number of items in the collection.

  function Is_Empty (C : Collection) return Boolean;
  --  Return True if and only if there are no items in the collection.

  function First (C : Collection) return Item;
  --  Return a copy of the item at the front of the collection.

  function Last (C : Collection) return Item;
  --  Return a copy of the item at the end of the collection.

  function Item_At (C : Collection; At_Index : Positive) return Item;
  --  Return a copy of the item at the indicated position in the
  --  collection.

  function Location (C : Collection; Elem : Item) return Natural;
  --  Return the first index at which the item is found (0 if the
  --  item desn't exist in the collecton).

  function New_Iterator
   (For_The_Collection : Collection) return Iterator'Class;
  --  Return a reset Iterator bound to the specific Collection.

private

  function Item_At
   (C : Collection; Index : Positive) return Item_Ptr;

  package Collection_Nodes
  is new BC.Support.Indefinite_Unbounded (Item => Item,
                        Item_Ptr => Item_Ptr,
                        Storage => Storage);

  type Collection is new Abstract_Ordered_Collection with record
    Rep : Collection_Nodes.Unb_Node;
  end record;

end BC.Indefinite_Containers.Collections.Ordered.Unbounded;
