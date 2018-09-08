{ 
  Copyright (C) 2014 Yann Mérignac

  This library is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 2.1 of the
  License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  As a special exception, the copyright holders of this library give
  you permission to link this library with independent modules to
  produce an executable, regardless of the license terms of these
  independent modules,and to copy and distribute the resulting
  executable under terms of your choice, provided that you also meet,
  for each linked independent module, the terms and conditions of the
  license of that module. An independent module is a module which is
  not derived from or based on this library. If you modify this
  library, you may extend this exception to your version of the
  library, but you are not obligated to do so. If you do not wish to
  do so, delete this exception statement from your version.

  You should have received a copy of the GNU Lesser General Public
  License along with this library. If not, see
  <http://www.gnu.org/licenses/>. 
} 
unit GContnrs;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

const
  MIN_BUCKET_COUNT = 4;
  MAX_BUCKET_COUNT = 1 shl 30;
  DEFAULT_HASHMAP_LOAD_FACTOR = 1.0;

type
  EContainerError = class(Exception);

  { TContainer }
  TContainer = class
  protected
    procedure RaiseContainerEmpty;
    procedure RaiseCursorDenotesWrongContainer;
    procedure RaiseCursorIsNil;
    procedure RaiseError(const Msg: String);
    procedure RaiseIndexOutOfRange;
    procedure RaiseItemAlreadyInSet;
    procedure RaiseItemNotInSet;
    procedure RaiseKeyAlreadyInMap;
    procedure RaiseKeyNotInMap;
    procedure RaiseMethodNotRedefined;
    procedure Unused(P: Pointer); inline;
  end;

  { TGenEnumerator }

  generic TGenEnumerator<_TItem_, _TPosition_> = class
  public type
    TGetCurrent = function(const Pos: _TPosition_) : _TItem_ of object;
    TMoveNext = function(var Pos:_TPosition_) : Boolean of object;
  private
    fGetter : TGetCurrent;
    fMover : TMoveNext;
    fPos : _TPosition_;

    function GetCurrent : _TItem_;
  public
    constructor Create(const Pos : _TPosition_; Mover: TMoveNext;
      Getter: TGetCurrent);
    function MoveNext: Boolean;
    property Current: _TItem_ read GetCurrent;
  end;

  { TAbstractVector }

  TAbstractVector = class(TContainer)
  protected
    fCapacity : Integer;
    fSize : Integer;

    procedure CheckIndex(Index: Integer); inline;
    procedure CheckIndexForAdd(Index: Integer); inline;
    procedure InsertSpaceFast(Position, Count: Integer); virtual; abstract;
    function ItemToString(Index: Integer) : String; virtual; abstract;
    procedure SetCapacity(ACapacity : Integer); virtual; abstract;
  public
    {** Removes all the items from the container. }
    procedure Clear;

    {** Deletes Count items begining at Position. }
    procedure Delete(Position: Integer; Count: Integer = 1);

    {** Deletes the first Count items. }
    procedure DeleteFirst(Count: Integer = 1);

    {** Deletes the last Count items. }
    procedure DeleteLast(Count: Integer = 1);

    {** Deletes all items in the range [PosFrom..PosTo]. }
    procedure DeleteRange(PosFrom, PosTo: Integer);

    {** Inserts Count undefined items at Position. }
    procedure InsertSpace(Position: Integer; Count: Integer = 1);

    {** Returns true if the container is empty. }
    function IsEmpty: Boolean; inline;

    {** Copies Count items from Src to Dst. }
    procedure Move(Src, Dst, Count: Integer); virtual; abstract;

    {** If necessary, increases the capacity of the container to ensure that it
      can hold at least MinCapacity items. }
    procedure Reserve(MinCapacity: Integer);

    {** Resizes the container to contain NewSize items. }
    procedure Resize(NewSize: Integer);

    {** Reorders the items in reverse order. }
    procedure Reverse;

    {** Reorders the items in the range [PosFrom..PosTo] in reverse order. }
    procedure ReverseRange(PosFrom, PosTo: Integer);

    {** Rearrange items randomly. }
    procedure Shuffle;

    {** Rearrange items in the range [PosFrom..PosTo] randomly. }
    procedure Shuffle(PosFrom, PosTo: Integer);

    {** Swaps the values of the items designated by I and J. }
    procedure Swap(I, J: Integer);

    {** Swaps the values of the items designated by I and J (no bounds check). }
    procedure SwapFast(I, J: Integer); virtual; abstract;

    {** Return a string representation for the container. }
    function ToString : String; override;

    {** Capacity of the container. }
    property Capacity : Integer read fCapacity;
    
    {** Number of items. }
    property Size: Integer read fSize;  
  end;
  
  { TGenVector }

  generic TGenVector<_TItem_> = class(TAbstractVector)
  public type
    PItem = ^_TItem_;
    TCompareItems = function (const A, B: _TItem_) : Integer of object;
    TItemToString = function (const Item: _TItem_) : String of object;
    TProcessItem = procedure(var Item: _TItem_) of object;
    TEnumerator = specialize TGenEnumerator<_TItem_, Integer>;

  strict private 
    fItems : array of _TItem_;
    fOnCompareItems: TCompareItems;
    fOnItemToString: TItemToString;

    function EnumeratorGet(const Pos: Integer) : _TItem_;
    function EnumeratorNext(var Pos: Integer) : Boolean;
    procedure Fill(Index, Count: Integer; const Value: _TItem_);
    function GetItemFast(Position: Integer) : _TItem_; inline;
    function GetItemPtrFast(Position: Integer): PItem;
    procedure InsertionSort(PosFrom, PosTo: Integer; Comparator: TCompareItems);
    procedure Quicksort(Left, Right: Integer; Comparator: TCompareItems);
    class procedure RealMove(Src, Dst: TGenVector;
      SrcFirst, DstFirst, Count: Integer);
    procedure SetOnCompareItems(AValue: TCompareItems);
    procedure SetOnItemToString(AValue: TItemToString);

  protected
    procedure InsertSpaceFast(Position, Count: Integer); override;
    function ItemToString(Index: Integer) : String; override;
    procedure SetCapacity(ACapacity : Integer); override;
  public
    {** Inserts Count times Item at the end of the container. }
    procedure Append(const Item: _TItem_);

    {** Inserts all the items of Src at the end of the container. }
    procedure AppendAll(Src: TGenVector);

    {** Inserts all the items of Src in the range [PosFrom..PosTo] at the end of
      the container. }
    procedure AppendRange(Src: TGenVector; PosFrom, PosTo: Integer);

    {** Searches for Item using the binary search algorithm. Returns the index of
      Item if its found. Otherwise, returns ( - InsertionPoint - 1 ).
      InsertionPoint is the point at which the key would be inserted into the
      container. }
    function BinarySearch(const Item: _TItem_) : Integer;
    function BinarySearch(const Item: _TItem_;
      Comparator: TCompareItems) : Integer;

    {** Searches for Item in range [PosFrom..PosTo] using the binary search
      algorithm. Returns the index of Item if its found. Otherwise, returns
      ( - InsertionPoint - 1 ). InsertionPoint is the point at which the key
      would be inserted into the range. }
    function BinarySearch(const Item: _TItem_;
      PosFrom, PosTo: Integer) : Integer;
    function BinarySearch(const Item: _TItem_;
      PosFrom, PosTo: Integer; Comparator: TCompareItems) : Integer;

    {** Returns true if the container contains Item. }
    function Contains(const Item: _TItem_) : Boolean;
    function Contains(const Item: _TItem_; Comparator: TCompareItems) : Boolean;

    {** Creates an empty vector and sets his capacity to InitialCapacity. }
    constructor Create(InitialCapacity: Integer = 16);

    function DefaultCompareItems(const A, B: _TItem_) : Integer; virtual;
    function DefaultItemToString(const Item: _TItem_) : String; virtual;

    {** Destroys the container. }
    destructor Destroy; override;

    {** If Obj = Self then returns true, else if Obj is not a TGenVector returns
      false, else returns true if Self and Obj contain the sames items. }
    function Equals(Obj: TObject) : Boolean; override;
    function Equals(Obj: TObject; Comparator: TCompareItems) : Boolean;

    {** Returns the index of the first item equal to Item or -1. }
    function FindIndex(const Item: _TItem_) : Integer;
    function FindIndex(const Item: _TItem_;
      Comparator: TCompareItems) : Integer;

    {** Returns a cursor on the first item equal to Item or NilCursor. The search
      starts at the element From.  }
    function FindIndex(const Item: _TItem_; PosFrom: Integer) : Integer;
    function FindIndex(const Item: _TItem_; PosFrom: Integer;
      Comparator: TCompareItems) : Integer;

    {** Returns the first Item. }
    function FirstItem : _TItem_; inline;

    function GetEnumerator : TEnumerator;

    {** Returns item at position Position. }
    function GetItem(Position: Integer) : _TItem_; inline;

    {** Returns a pointer designating item at position Position. }
    function GetItemPtr(Position: Integer): PItem;

    {** Inserts Count times Item before Before. }
    procedure Insert(Before: Integer; const Item: _TItem_;
      Count: Integer = 1);

    {** Inserts all the items of Src before Before. }
    procedure InsertAll(Before: Integer; Src: TGenVector);

    {** Inserts before Before all the items of Src in the range
      [PosFrom..PosTo]. }
    procedure InsertRange(Before: Integer; Src: TGenVector;
      PosFrom, PosTo: Integer);

    {** Returns true if the items are sorted. }
    function IsSorted : Boolean;
    function IsSorted(Comparator: TCompareItems): Boolean;

    {** Invokes Process on each items. }
    procedure Iterate(Process: TProcessItem);

    {** Invokes Process on each items in range [PosFrom..PosTo]. }
    procedure Iterate(Process: TProcessItem; const PosFrom, PosTo: Integer);

    {** Returns the last Item. }
    function LastItem: _TItem_; inline;

    {** Returns index of the greatest item. }
    function MaxPos : Integer;
    function MaxPos(Comparator: TCompareItems) : Integer;

    {** Returns index of the greatest item in the range [PosFrom..PosTo]. }
    function MaxPos(PosFrom, PosTo: Integer) : Integer;
    function MaxPos(PosFrom, PosTo: Integer;
      Comparator: TCompareItems) : Integer;

    {** Removes items from Src and inserts them into Self. Afterwards, Self
      contains the union of the items that were initially in Src and Self. Src
      is left empty. If Self and Src are initially sorted, then Self is
      sorted. }
    procedure Merge(Src: TGenVector);
    procedure Merge(Src: TGenVector; Comparator: TCompareItems);

    {** Returns index of the lowest item. }
    function MinPos : Integer;
    function MinPos(Comparator: TCompareItems) : Integer;

    {** Returns index of the lowest item in the range [PosFrom..PosTo]. }
    function MinPos(PosFrom, PosTo: Integer) : Integer;
    function MinPos(PosFrom, PosTo: Integer;
      Comparator: TCompareItems) : Integer;

    {** Copies Count items from Src to Dst. }
    procedure Move(Src, Dst, Count: Integer); override;

    {** Inserts Count times Item at the begining of the container. }
    procedure Prepend(const Item: _TItem_; Count: Integer = 1);

    {** Inserts all the items of Src at the begining of the container. }
    procedure PrependAll(Src: TGenVector);

    {** Inserts all the items of Src in the range [PosFrom..PosTo] at the
      begining of the container. }
    procedure PrependRange(Src: TGenVector; PosFrom, PosTo: Integer);

    procedure ReadFirstItem(out Value : _TItem_); inline;

    procedure ReadItem(Position: Integer; out Value: _TItem_);

    procedure ReadItemFast(Position: Integer; out Value: _TItem_); inline;

    procedure ReadLastItem(out Value : _TItem_); inline;

    {** Replaces items in range [Index..Index + Count - 1] by Value. }
    procedure Replace(Index, Count: Integer; const Value: _TItem_);

    {** Returns the index of the first item equal to Item or -1. }
    function ReverseFindIndex(const Item: _TItem_) : Integer;
    function ReverseFindIndex(const Item: _TItem_;
      Comparator: TCompareItems) : Integer;

    {** Returns a cursor on the first item equal to Item or NilCursor. The search
      starts at the element From.  }
    function ReverseFindIndex(const Item: _TItem_; PosFrom: Integer) : Integer;
    function ReverseFindIndex(const Item: _TItem_;
      PosFrom: Integer; Comparator: TCompareItems) : Integer;

    {** Assigns the value Value to the item at Position. }
    procedure SetItem(Position: Integer; const Value: _TItem_); inline;

    procedure SetItemFast(Position: Integer; const Value: _TItem_); inline;

    {** Sorts the items. }
    procedure Sort;
    procedure Sort(Comparator: TCompareItems);

    {** Sorts the items in the range [PosFrom..PosTo]. }
    procedure Sort(PosFrom, PosTo: Integer);
    procedure Sort(PosFrom, PosTo: Integer; Comparator: TCompareItems);

    {** Swaps the values of the items designated by I and J (no bounds check). }
    procedure SwapFast(I, J: Integer); override;

    {** Provides access to the items in the container. }
    property Items[Index: Integer] : _TItem_ read GetItemFast
      write SetItemFast; default;

    {** Provides access to pointers on the items in the container. }
    property ItemsPtr[Index: Integer] : PItem read GetItemPtrFast;

    property OnCompareItems : TCompareItems read fOnCompareItems
      write SetOnCompareItems;

    property OnItemToString : TItemToString read fOnItemToString
      write SetOnItemToString;
  end;  
  
  { TGenDeque }

  generic TGenDeque<_TItem_> = class(TAbstractVector)
  public type
    PItem = ^_TItem_;
    TCompareItems = function (const A, B: _TItem_) : Integer of object;
    TItemToString = function (const Item: _TItem_) : String of object;
    TProcessItem = procedure(var Item: _TItem_) of object;
    TEnumerator = specialize TGenEnumerator<_TItem_, Integer>;

  strict private
    fItems : array of _TItem_;
    fOnCompareItems: TCompareItems;
    fOnItemToString: TItemToString;
    fStart : Integer;

    procedure DecRank(var Rank: Integer); inline;
    function Equals(Deque: TGenDeque; Comparator: TCompareItems): Boolean;
    function EnumeratorGet(const Pos: Integer) : _TItem_;
    function EnumeratorNext(var Pos: Integer) : Boolean;
    procedure Fill(Index, Count: Integer; const Value: _TItem_);
    function GetItemPtrFast(Position: Integer): PItem;
    procedure IncRank(var Rank: Integer); inline;
    procedure IncreaseCapacity(ACapacity : Integer);
    function IndexToRank(Index: Integer) : Integer; inline;
    procedure InsertionSort(PosFrom, PosTo: Integer; Comparator: TCompareItems);
    procedure Quicksort(Left, Right: Integer; Comparator: TCompareItems);
    function RankToIndex(Rank: Integer) : Integer; inline;
    class procedure RealMoveIndex(Src, Dst: TGenDeque;
      SrcFirst, DstFirst, Count: Integer);
    procedure RealMoveRank(Src, Dst, Count: Integer);
    procedure ReduceCapacity(ACapacity : Integer);
    procedure SetOnCompareItems(AValue: TCompareItems);
    procedure SetOnItemToString(AValue: TItemToString);
  
  protected
    procedure InsertSpaceFast(Position, Count: Integer); override;
    function ItemToString(Index: Integer) : String; override;
    procedure SetCapacity(ACapacity : Integer); override;
  public
    {** Inserts Count times Item at the end of the container. }
    procedure Append(const Item: _TItem_; Count: Integer = 1);

    {** Inserts all the items of Src at the end of the container. }
    procedure AppendAll(Src: TGenDeque);

    {** Inserts all the items of Src in the range [PosFrom..PosTo] at the end of
      the container. }
    procedure AppendRange(Src: TGenDeque; PosFrom, PosTo: Integer);

    {** Searches for Item using the binary search algorithm. Returns the index of
      Item if its found. Otherwise, returns ( - InsertionPoint - 1 ).
      InsertionPoint is the point at which the key would be inserted into the
      container. }
    function BinarySearch(const Item: _TItem_) : Integer;
    function BinarySearch(const Item: _TItem_; Comparator: TCompareItems) : Integer;

    {** Searches for Item in range [PosFrom..PosTo] using the binary search
      algorithm. Returns the index of Item if its found. Otherwise, returns
      ( - InsertionPoint - 1 ). InsertionPoint is the point at which the key
      would be inserted into the range. }
    function BinarySearch(const Item: _TItem_; PosFrom, PosTo: Integer) : Integer;
    function BinarySearch(const Item: _TItem_;
      PosFrom, PosTo: Integer; Comparator: TCompareItems) : Integer;

    {** Returns true if the container contains Item. }
    function Contains(const Item: _TItem_) : Boolean;
    function Contains(const Item: _TItem_; Comparator: TCompareItems) : Boolean;

    {** Creates an empty deque and sets his capacity to InitialCapacity. }
    constructor Create(InitialCapacity: Integer = 16);

    function DefaultCompareItems(const A, B: _TItem_) : Integer; virtual;
    function DefaultItemToString(const Item: _TItem_) : String; virtual;

    {** Destroys the container. }
    destructor Destroy; override;

    {** If Obj = Self then returns @true, else if Obj is not a TGenDeque returns
      false, else returns @true if Self and Obj contain the sames items. }
    function Equals(Obj: TObject) : Boolean; override;
    function Equals(Obj: TObject; Comparator: TCompareItems) : Boolean;

    {** Returns the index of the first item equal to Item or -1. }
    function FindIndex(const Item: _TItem_) : Integer;
    function FindIndex(const Item: _TItem_; Comparator: TCompareItems) : Integer;

    {** Returns a cursor on the first item equal to Item or NilCursor. The search
      starts at the element From.  }
    function FindIndex(const Item: _TItem_; PosFrom: Integer) : Integer;
    function FindIndex(const Item: _TItem_; PosFrom: Integer; Comparator: TCompareItems) : Integer;

    {** Returns the first Item. }
    function FirstItem : _TItem_; inline;

    function GetEnumerator : TEnumerator;

    function GetItemFast(Position: Integer) : _TItem_; inline;

    {** Returns item at position Position. }
    function GetItem(Position: Integer) : _TItem_; inline;

    {** Returns a pointer designating item at position Position. }
    function GetItemPtr(Position: Integer): PItem;

    {** Inserts Count times Item before Before. }
    procedure Insert(Before: Integer; const Item: _TItem_;
      Count: Integer = 1);

    {** Inserts all the items of Src before Before. }
    procedure InsertAll(Before: Integer; Src: TGenDeque);

    {** Inserts before Before all the items of Src in the range
      [PosFrom..PosTo]. }
    procedure InsertRange(Before: Integer; Src: TGenDeque;
      PosFrom, PosTo: Integer);

    {** Returns true if the items are sorted. }
    function IsSorted: Boolean;
    function IsSorted(Comparator: TCompareItems): Boolean;

    {** Invokes Process on each items. }
    procedure Iterate(Process: TProcessItem);

    {** Invokes Process on each items in range [PosFrom..PosTo]. }
    procedure Iterate(Process: TProcessItem; const PosFrom, PosTo: Integer);

    {** Returns the last Item. }
    function LastItem: _TItem_; inline;

    {** Returns index of the greatest item. }
    function MaxPos : Integer;
    function MaxPos(Comparator: TCompareItems) : Integer;

    {** Returns index of the greatest item in the range [PosFrom..PosTo]. }
    function MaxPos(PosFrom, PosTo: Integer) : Integer;
    function MaxPos(PosFrom, PosTo: Integer; Comparator: TCompareItems) : Integer;

    {** Removes items from Src and inserts them into Self. Afterwards, Self
      contains the union of the items that were initially in Src and Self. Src
      is left empty. If Self and Src are initially sorted, then Self is
      sorted. }
    procedure Merge(Src: TGenDeque);
    procedure Merge(Src: TGenDeque; Comparator: TCompareItems);

    {** Returns index of the lowest item. }
    function MinPos : Integer;
    function MinPos(Comparator: TCompareItems) : Integer;

    {** Returns index of the lowest item in the range [PosFrom..PosTo]. }
    function MinPos(PosFrom, PosTo: Integer) : Integer;
    function MinPos(PosFrom, PosTo: Integer; Comparator: TCompareItems) : Integer;

    {** Copies Count items from Src to Dst. }
    procedure Move(Src, Dst, Count: Integer); override;

    {** Inserts Count times Item at the begining of the container. }
    procedure Prepend(const Item: _TItem_; Count: Integer = 1);

    {** Inserts all the items of Src at the begining of the container. }
    procedure PrependAll(Src: TGenDeque);

    {** Inserts all the items of Src in the range [PosFrom..PosTo] at the
      begining of the container. }
    procedure PrependRange(Src: TGenDeque; PosFrom, PosTo: Integer);

    procedure ReadFirstItem(out Value : _TItem_); inline;

    procedure ReadItem(Position: Integer; out Value: _TItem_);

    procedure ReadItemFast(Position: Integer; out Value: _TItem_); inline;

    procedure ReadLastItem(out Value : _TItem_); inline;

    {** Replaces items in range [Index..Index + Count - 1] by Value. }
    procedure Replace(Index, Count: Integer; const Value: _TItem_);

    {** Returns the index of the first item equal to Item or -1. }
    function ReverseFindIndex(const Item: _TItem_) : Integer;
    function ReverseFindIndex(const Item: _TItem_; Comparator: TCompareItems) : Integer;

    {** Returns a cursor on the first item equal to Item or NilCursor. The search
      starts at the element From.  }
    function ReverseFindIndex(const Item: _TItem_; PosFrom: Integer) : Integer;
    function ReverseFindIndex(const Item: _TItem_; PosFrom: Integer;
      Comparator: TCompareItems) : Integer;

    {** Assigns the value Value to the item at Position. }
    procedure SetItem(Position: Integer; const Value: _TItem_); inline;

    procedure SetItemFast(Position: Integer; const Value: _TItem_); inline;

    {** Sorts the items. }
    procedure Sort;
    procedure Sort(Comparator: TCompareItems);

    {** Sorts the items in the range [PosFrom..PosTo]. }
    procedure Sort(PosFrom, PosTo: Integer);
    procedure Sort(PosFrom, PosTo: Integer; Comparator: TCompareItems);

    procedure SwapFast(I, J: Integer); override;

    {** Provides access to the items in the container. }
    property Items[Index: Integer] : _TItem_ read GetItemFast
      write SetItemFast; default;

    {** Provides access to pointers on the items in the container. }
    property ItemsPtr[Index: Integer] : PItem read GetItemPtrFast;

    property OnCompareItems : TCompareItems read fOnCompareItems
      write SetOnCompareItems;

    property OnItemToString : TItemToString read fOnItemToString
      write SetOnItemToString;
  end;  

  TAbstractList = class;

  { TListCursor }

  TListCursor = object
  strict private
    fList : TAbstractList;
    fNode : Pointer;

  public
    {** Check if the cursors designate the same item. }
    function Equals(const Cursor: TListCursor) : Boolean; inline;

    {** Check if the cursors designate an item. }
    function HasItem: Boolean; inline;

    constructor Init(AList : TAbstractList; ANode: Pointer);

    {** Returns true if the cursor designates the first element. }
    function IsFirst: Boolean; inline;

    {** Returns true if the cursor designates the last element. }
    function IsLast: Boolean; inline;

    {** Equivalent to not HasItem. }
    function IsNil: Boolean; inline;

    {** If cursor is nil then do nothing, else if cursor is last then cursor
      becomes nil cursor, otherwise move cursor to the next item.  }
    procedure MoveNext; inline;

    {** If cursor is nil then do nothing, else if cursor is first then cursor
      becomes nil cursor, otherwise move cursor to the previous item.  }
    procedure MovePrevious; inline;

    {** The designated List. }
    property List : TAbstractList read fList;

    {** The designated node. }
    property Node : Pointer read fNode write fNode;
  end;

  { TAbstractList }

  TAbstractList = class(TContainer)
  protected
    procedure CheckValid(const Cursor: TListCursor);
    procedure CheckNotNil(const Cursor: TListCursor);
    function CursorIsFirst(const Cursor: TListCursor) : Boolean; virtual; abstract;
    function CursorIsLast(const Cursor: TListCursor) : Boolean; virtual; abstract;
    procedure CursorMoveNext(var Cursor: TListCursor); virtual; abstract;
    procedure CursorMovePrev(var Cursor: TListCursor); virtual; abstract;
  end;

  { TGenList }

  generic TGenList<_TItem_> = class(TAbstractList)
  public type
    PItem = ^_TItem_;
    TCompareItems = function (const A, B: _TItem_) : Integer of object;
    TItemToString = function (const Item: _TItem_) : String of object;
    TProcessItem = procedure(var Item: _TItem_) of object;
    TEnumerator = specialize TGenEnumerator<_TItem_, TListCursor>;

  strict private type
    PNode = ^TNode;
    TNode = record
      Item : _TItem_;
      Next, Previous : PNode;
    end;

  strict private
    fHead : PNode;
    fOnCompareItems: TCompareItems;
    fOnItemToString: TItemToString;
    fTail : PNode;
    fSize : Integer;
    fNilCursor : TListCursor;

    procedure DeleteNodesBackward(From: PNode; Count: Integer);
    procedure DeleteNodesBetween(NodeFrom, NodeTo: PNode);
    procedure DeleteNodesForward(From: PNode; Count: Integer);
    function EnumeratorGet(const Pos: TListCursor) : _TItem_;
    function EnumeratorNext(var Pos: TListCursor) : Boolean;
    function Equals(List: TGenList; Comparator: TCompareItems) : Boolean;
    function GetItemFast(const Position: TListCursor) : _TItem_; inline;
    function GetItemPtrFast(const Position: TListCursor) : PItem; inline;
    procedure InsertItem(const Item: _TItem_; Pos: PNode; Count: Integer);
    procedure Partition(Pivot, Back: PNode; Comparator: TCompareItems);
    procedure RealSort(Front, Back: PNode; Comparator: TCompareItems);
    procedure SetOnCompareItems(AValue: TCompareItems);
    procedure SetOnItemToString(AValue: TItemToString);
    procedure SpliceNodes(Before, PosFrom, PosTo: PNode);

  protected
    function CursorIsFirst(const Cursor: TListCursor) : Boolean; override;
    function CursorIsLast(const Cursor: TListCursor) : Boolean; override;
    procedure CursorMoveNext(var Cursor: TListCursor); override;
    procedure CursorMovePrev(var Cursor: TListCursor); override;

  public
    {** Inserts Count times Item at the end of the container. }
    procedure Append(const Item: _TItem_; Count: Integer = 1);

    {** Inserts all the items of Src at the end of the container. }
    procedure AppendAll(Src: TGenList);

    {** Inserts all the items of Src in the range [PosFrom..PosTo] at the end of
      the container. }
    procedure AppendRange(Src: TGenList; const PosFrom, PosTo: TListCursor);

    {** Removes all the items from the container. }
    procedure Clear;

    {** Returns true if the container contains Item. }
    function Contains(const Item: _TItem_) : Boolean;
    function Contains(const Item: _TItem_; Comparator: TCompareItems) : Boolean;

    {** Creates an empty list. }
    constructor Create;

    function DefaultCompareItems(const A, B: _TItem_) : Integer; virtual;
    function DefaultItemToString(const Item: _TItem_) : String; virtual;

    {** Deletes Count items begining at Position and then sets Position to
      NilCursor. }
    procedure Delete(var Position: TListCursor; Count: Integer = 1);

    {** Deletes the first Count items. }
    procedure DeleteFirst(Count: Integer = 1);

    {** Deletes the last Count items. }
    procedure DeleteLast(Count: Integer = 1);

    {** Deletes all items in the range [PosFrom..PosTo]. }
    procedure DeleteRange(const PosFrom, PosTo: TListCursor);

    {** Destroys the container. }
    destructor Destroy; override;

    {** If Obj = Self then returns true, else if Obj is not a TGenList returns false,
      else returns true if Self and Obj contain the sames items. }
    function Equals(Obj: TObject) : Boolean; override;
    function Equals(Obj: TObject; Comparator: TCompareItems) : Boolean;

    {** Returns a cursor on the first item equal to Item or NilCursor. }
    function Find(const Item: _TItem_) : TListCursor;
    
    function Find(const Item: _TItem_; Comparator: TCompareItems) : TListCursor;

    {** Returns a cursor on the first item equal to Item or NilCursor.The search
      starts at the first element if Position is NilCursor, and at the element
      designated by Position otherwise.  }
    function Find(const Item: _TItem_; const Position: TListCursor) : TListCursor;

    function Find(const Item: _TItem_; const Position: TListCursor; Comparator: TCompareItems): TListCursor;

    {** Returns a cursor that designates the first element of the container or
      NilCursor if the container is empty. }
    function First: TListCursor;

    {** Returns the first Item. }
    function FirstItem : _TItem_; inline;

    {** If Index is not in the range [0..Size - 1], then returns NilCursor.
      Otherwise, returns a cursor designating the item at position Index. }
    function GetCursor(Index: Integer): TListCursor;

    function GetEnumerator : TEnumerator;

    {** Returns the item designated by Position. }
    function GetItem(const Position: TListCursor) : _TItem_; inline;

    {** Returns a pointer designating the item designated by Position. }
    function GetItemPtr(const Position: TListCursor) : PItem; inline;

    {** Inserts Count times Item before Before. }
    procedure Insert(const Before: TListCursor; const Item: _TItem_;
      Count: Integer = 1);

    {** Inserts Count times Item before Before. Position designates the first
      newly-inserted element. }
    procedure Insert(const Before: TListCursor; const Item: _TItem_;
      out Position: TListCursor; Count: Integer);

    {** Inserts all the items of Src before Before. }
    procedure InsertAll(const Before: TListCursor; Src: TGenList);

    {** Inserts before Before all the items of Src in the range
      [PosFrom..PosTo]. }
    procedure InsertRange(const Before : TListCursor; Src: TGenList;
      const PosFrom, PosTo: TListCursor);

    {** Returns true if the list is empty. }
    function IsEmpty: Boolean; inline;

    {** Returns @true if the items are sorted. }
    function IsSorted : Boolean;

    function IsSorted(Comparator: TCompareItems) : Boolean;

    procedure Iterate(Process: TProcessItem);

    procedure Iterate(Process: TProcessItem; const PosFrom, PosTo: TListCursor);

    {** Returns a cursor that designates the last element of the container or
      NilCursor if the container is empty. }
    function Last: TListCursor;

    {** Returns the last Item. }
    function LastItem: _TItem_; inline;

    {** Removes items from Src and inserts them into Self. Afterwards, Self
      contains the union of the items that were initially in Src and Self. Src
      is left empty. If Self and Src are initially sorted, then Self is
      sorted. }
    procedure Merge(Src: TGenList);
    procedure Merge(Src: TGenList; Comparator: TCompareItems);

    {** Inserts Count times Item at the begining of the container. }
    procedure Prepend(const Item: _TItem_; Count: Integer = 1);

    {** Inserts all the items of Src at the begining of the container. }
    procedure PrependAll(Src: TGenList);

    {** Inserts all the items of Src in the range [PosFrom..PosTo] at the
      begining of the container. }
    procedure PrependRange(Src: TGenList; const PosFrom, PosTo: TListCursor);

    procedure ReadFirstItem(out Value : _TItem_); inline;

    procedure ReadItem(const Position: TListCursor; out Value: _TItem_);

    procedure ReadItemFast(const Position: TListCursor; out Value: _TItem_); inline;

    procedure ReadLastItem(out Value : _TItem_); inline;

    {** Replaces items in range [Position..Position + Count - 1] by Value. }
    procedure Replace(const Position: TListCursor; Count: Integer;
      const Value: _TItem_);

    {** Reorders the items in reverse order. }
    procedure Reverse;

    {** Returns a cursor on the first item equal to Item or NilCursor. }
    function ReverseFind(const Item: _TItem_) : TListCursor;
    function ReverseFind(const Item: _TItem_; Comparator: TCompareItems): TListCursor;

    {** Returns a cursor on the first item equal to Item or NilCursor.The search
      starts at the last element if Position is NilCursor, and at the element
      designated by Position otherwise.  }
    function ReverseFind(const Item: _TItem_; const Position: TListCursor) : TListCursor;
    function ReverseFind(const Item: _TItem_; const Position: TListCursor;
      Comparator: TCompareItems) : TListCursor;

    {** Reorders the items in the range [PosFrom..PosTo] in reverse order. }
    procedure ReverseRange(const PosFrom, PosTo: TListCursor);

    {** Assigns the value Value to the item designated by Position. }
    procedure SetItem(const Position: TListCursor; const Value: _TItem_);

    procedure SetItemFast(const Position: TListCursor; const Value: _TItem_); inline;

    {** Sorts the items. }
    procedure Sort;
    procedure Sort(Comparator: TCompareItems);

    {** Sorts the items in the range [PosFrom..PosTo]. }
    procedure Sort(const PosFrom, PosTo: TListCursor);
    procedure Sort(const PosFrom, PosTo: TListCursor; Comparator: TCompareItems);

    {** Removes all items of Src and moves them to Self before Before. }
    procedure Splice(const Before: TListCursor; Src: TGenList);

    {** Removes from Src the item designated by Position and moves it to Self
      before Before. }
    procedure Splice(const Before: TListCursor; Src: TGenList;
      const Position: TListCursor);

    {** Removes all items of Src in the range [SrcFrom..SrcTo] and moves them to
      Self before Before. }
    procedure Splice(const Before: TListCursor; Src: TGenList;
      const SrcFrom, SrcTo: TListCursor);

    {** Swaps the values of the items designated by I and J. }
    procedure Swap(const I, J: TListCursor);

    {** Swaps the nodes designated by I and J. }
    procedure SwapLinks(const I, J: TListCursor);

    {** Return a string representation for the container. }
    function ToString : String; override;

    {** Provides access to the items in the container. }
    property Items[const Index: TListCursor] : _TItem_
      read GetItemFast write SetItemFast; default;

    {** Provides access to pointers on the items in the container. }
    property ItemsPtr[const Index: TListCursor] : PItem read GetItemPtrFast;

    {** A nil cursor. }
    property NilCursor: TListCursor read fNilCursor;

    property OnCompareItems : TCompareItems read fOnCompareItems
      write SetOnCompareItems;

    property OnItemToString : TItemToString read fOnItemToString
      write SetOnItemToString;

    {** Number of elements in the list. }
    property Size: Integer read fSize;
  end;

  { TGenPriorityQueue }

  generic TGenPriorityQueue<_TItem_> = class(TContainer)
  public type
    TCompareItems = function (const A, B: _TItem_) : Integer of object;

  strict private
    fCapacity : Integer;
    fItems : array of _TItem_;
    fOnCompareItems: TCompareItems;
    fSize : Integer;

    procedure SetOnCompareItems(AValue: TCompareItems);
    procedure MoveDown(Index: Integer; const Item: _TItem_);
    procedure MoveUp(Index: Integer; const Item: _TItem_);

  public
    {** Empty the queue of all items. }
    procedure Clear;

    {** Creates an empty priority queue. }
    constructor Create(InitialCapacity : Integer = 16);

    function DefaultCompareItems(const A, B: _TItem_) : Integer; virtual;

    {** Returns true if the priority queue is empty. }
    function IsEmpty: Boolean; inline;

    {** Frees unused memory. }
    procedure Pack;

    {** Removes the item from the top of the stack. }
    procedure Pop;

    {** Adds Item to the top of the stack. }
    procedure Push(const Item: _TItem_);

    procedure ReadTop(out Value: _TItem_);

    {** If necessary, increases the capacity of the container to ensure that it
      can hold at least MinCapacity items. }
    procedure Reserve(MinCapacity : Integer);

    {** Returns the item at the top of the stack. }
    function Top : _TItem_;

    {** Capacity of the container. }
    property Capacity : Integer read fCapacity;

    property OnCompareItems : TCompareItems read fOnCompareItems write SetOnCompareItems;

    {** Number of elements. }
    property Size: Integer read fSize;
  end;

  { TGenQueue }

  generic TGenQueue<_TItem_, _TContainer_> = class(TContainer)
  private
    fData : _TContainer_;
    function GetSize: Integer; inline;

  public
    {** Add the item to the back of the queue. }
    procedure Append(const Item: _TItem_);

    {** Empty the queue of all items. }
    procedure Clear;

    {** Creates an empty queue. }
    constructor Create;

    {** Destroys the container. }
    destructor Destroy; override;

    {** Returns a copy of the item at the front of the queue. }
    function Front : _TItem_;

    {** Returns true if the queue is empty. }
    function IsEmpty: Boolean; inline;

    {** Removes the item from the front of the queue. }
    procedure Pop;

    procedure ReadFront(out Value: _TItem_);

    {** Number of items. }
    property Size : Integer read GetSize;
  end;

  { TGenStack }

  generic TGenStack<_TItem_, _TContainer_> = class(TContainer)
  private
    fData : _TContainer_;
    function GetSize: Integer; inline;

  public
    {** Removes all the items from the stack. }
    procedure Clear;

    {** Creates an empty stack. }
    constructor Create;

    {** Destroys the stack. }
    destructor Destroy; override;

    {** Returns true if the stack is empty. }
    function IsEmpty: Boolean; inline;

    {** Removes the item from the top of the stack. }
    procedure Pop;

    {** Adds Item to the top of the stack. }
    procedure Push(const Item: _TItem_);

    procedure ReadTop(out Value : _TItem_);

    {** Returns the item at the top of the stack. }
    function Top : _TItem_;

    {** Number of items. }
    property Size : Integer read GetSize;
  end;

  TAbstractHashMap = class;

  { THashMapCursor }

  THashMapCursor = object
    strict private
      fBucket : Integer;
      fHashMap : TAbstractHashMap;
      fEntry : Pointer;
      fPrevious : Pointer;

    public
      {** Check if the cursors designate the same item. }
      function Equals(const Cursor: THashMapCursor) : Boolean; inline;

      {** Check if the cursors designate an item. }
      function HasItem: Boolean; inline;

      {** Constructor. }
      constructor Init(HashMap : TAbstractHashMap; BucketNum: Integer;
        AEntry, APrevious: Pointer);

      {** Returns true if the cursor designates the first element. }
      function IsFirst: Boolean; inline;

      {** Returns true if the cursor designates the last element. }
      function IsLast: Boolean; inline;

      {** Equivalent to not HasItem. }
      function IsNil: Boolean; inline;

      {** If cursor is nil then do nothing, else if cursor is last then cursor
        becomes nil cursor, otherwise move cursor to the next item.  }
      procedure MoveNext; inline;

      {** Designated bucket. }
      property Bucket : Integer read fBucket write fBucket;

      property HashMap : TAbstractHashMap read fHashMap;

      {** Designated entry. }
      property Entry : Pointer read fEntry write fEntry;

      property Previous : Pointer read fPrevious write fPrevious;
  end;

  { TAbstractHashMap }

  TAbstractHashMap = class(TContainer)
  protected
    function CursorIsFirst(const Cursor: THashMapCursor): Boolean; virtual; abstract;
    function CursorIsLast(const Cursor: THashMapCursor): Boolean; virtual; abstract;
    procedure CursorMoveNext(const Cursor: THashMapCursor); virtual; abstract;
  end;

  { TGenHashMap }

  generic TGenHashMap<_TKey_, _TItem_> = class(TAbstractHashMap)
  public type
    THashKey = function (const Key: _TKey_) : Integer of object;
    TItemToString = function (const Item: _TItem_) : String of object;
    TKeysEqual = function (const A, B: _TKey_) : Boolean of object;
    TKeyToString = function (const Key: _TKey_) : String of object;
    TEnumerator = specialize TGenEnumerator<_TItem_, THashMapCursor>;

  private type
    PPEntry = ^PEntry;
    PEntry = ^TEntry;
    TEntry = record
      Key : _TKey_;
      Value : _TItem_;
      Next : PEntry;
    end;

  strict private
    fBucketCount : Integer;
    fBuckets : PPEntry;
    fFirstNonEmptyBucket: Integer;
    fLastNonEmptyBucket : Integer;
    fMaxBucketCount : Integer;
    fMaxLoadFactor : Real;
    fNilCursor : THashMapCursor;
    fOnHashKey: THashKey;
    fOnItemToString: TItemToString;
    fOnKeysEqual: TKeysEqual;
    fOnKeyToString: TKeyToString;
    fSize : Integer;
    fThreshold : Integer;

    procedure AppendBuckets(Count: Integer);
    function CollectEntries : PEntry;

    procedure DeleteEntry(Bucket : Integer; Entry, Previous: PEntry);
    procedure DisposeEntries(E: PEntry);
    function EnumeratorGet(const Pos: THashMapCursor) : _TItem_;
    function EnumeratorNext(var Pos: THashMapCursor) : Boolean;
    function FindEntry(Bucket: Integer; const Key: _TKey_) : PEntry;
    function FindEntry(Bucket: Integer; const Key: _TKey_; out Previous : PEntry) : PEntry;
    function GetEntry(const Key: _TKey_): PEntry; inline;
    function GetEntryAt(const Position: THashMapCursor): PEntry; inline;
    function GetLoadFactor: Real;
    function IndexFor(Hash: Integer) : Integer; inline;
    procedure InsertCollectedEntries(CollectedEntries: PEntry);
    procedure InsertEntry(Bucket: Integer; Entry: PEntry);
    procedure InsertEntry(Entry, Before: PEntry);
    function NextNonEmptyBucket(Bucket: Integer) : Integer;
    function NewEntry(const Key: _TKey_; const Value: _TItem_) : PEntry; inline;
    procedure NilifyBuckets(BucketFrom, Count: Integer);
    function PreviousNonEmptyBucket(Bucket: Integer) : Integer;
    procedure Resize(NewCapacity: Integer);
    procedure SetOnHashKey(AValue: THashKey);
    procedure SetOnItemToString(AValue: TItemToString);
    procedure SetOnKeysEqual(AValue: TKeysEqual);
    procedure SetOnKeyToString(AValue: TKeyToString);

  protected
    function CursorIsFirst(const Cursor: THashMapCursor): Boolean; override;
    function CursorIsLast(const Cursor: THashMapCursor): Boolean; override;
    procedure CursorMoveNext(const Cursor: THashMapCursor); override;
  public
    {** Removes all the items from the container. }
    procedure Clear;

    {** Returns true if the container contains Key. }
    function Contains(const Key : _TKey_) : Boolean;

    {** Creates an empty hash map and sets his capacity to InitialCapacity. }
    constructor Create(InitialCapacity: Integer = MIN_BUCKET_COUNT);

    {** Creates an empty hash map and sets his load factor to MaxLoadFact. }
    constructor Create(MaxLoadFact: Real);

    {** Creates an empty hash map and sets his capacity to InitialCapacity and
      his load factor to MaxLoadFact. }
    constructor Create(InitialCapacity: Integer; MaxLoadFact: Real);

    function DefaultHashKey(const Key: _TKey_) : Integer; virtual;
    function DefaultItemToString(const Item: _TItem_) : String; virtual;
    function DefaultKeysEqual(const A, B: _TKey_) : Boolean; virtual;
    function DefaultKeyToString(const Key: _TKey_) : String; virtual;

    {** Checks if an item with the key Key is present. If a match is found,
      removes the item from the map. Otherwise raises an exception. }
    procedure Delete(const Key: _TKey_);

    {** Deletes the item designated by Position. }
    procedure DeleteAt(const Position: THashMapCursor);

    {** Destroys the container. }
    destructor Destroy; override;

    {** Checks if an item with the key Key is present. If a match is found,
      removes the item from the map. }
    procedure Exclude(const Key : _TKey_);

    {** Checks if an item associated with Key is present. If a match is found,
      a cursor designating the matching item is returned. Otherwise,
      NilCursor is returned. }
    function Find(const Key : _TKey_) : THashMapCursor;

    {** Returns a cursor that designates the first element of the container
      or NilCursor if the container is empty. }
    function First: THashMapCursor;

    function GetEnumerator : TEnumerator;

    function GetItem(const Key: _TKey_): _TItem_;

    function GetItemAt(const Position: THashMapCursor): _TItem_;

    function GetKeyAt(const Position : THashMapCursor) : _TKey_;

    {** Inserts Key and Value into the map. If an entry with the same Key is
      already in the map, then the old value is replaced. }
    procedure Include(const Key : _TKey_; const Value: _TItem_);

    {** Inserts Key and Value into the map. If an entry with the same Key is
      already in the map, then an exception is raised. }
    procedure Insert(const Key : _TKey_; const Value: _TItem_);

    {** If an entry with the key Key is already in the map, then Inserted is set
      to false. Otherwise, Insert inserts Key and Value into the map and sets
      Inserted to true. }
    procedure Insert(const Key : _TKey_; const Value: _TItem_;
      out Inserted: Boolean);

    {** Returns true if the container is empty. }
    function IsEmpty: Boolean; inline;

    procedure ReadItem(const Key: _TKey_; out Value: _TItem_);
    procedure ReadItemAt(const Position: THashMapCursor; out Value: _TItem_);
    procedure ReadKeyAt(const Position : THashMapCursor; out Key: _TKey_);

    {** Checks if an entry with the key Key is present. If a match is found,
      assigns Key and Value to the matching entry. Otherwise, an exception is
      raised. }
    procedure Replace(const Key : _TKey_; const Value: _TItem_);

    procedure SetItemAt(const Position: THashMapCursor; AValue: _TItem_);

    {** Return a string representation for the container. }
    function ToString : String; override;

    property BucketCount : Integer read fBucketCount;

    {** Provides access to the items in the container. }
    property ItemAt[const Position: THashMapCursor] : _TItem_ read GetItemAt
      write SetItemAt;

    {** Provides access to the items in the container. }
    property Items[const Key: _TKey_] : _TItem_ read GetItem write Include;
      default;

    {** Provides access to the keys in the container. }
    property Keys[const Position: THashMapCursor] : _TKey_ read GetKeyAt;

    property LoadFactor : Real read GetLoadFactor;

    property MaxBucketCount : Integer read fMaxBucketCount;

    property MaxLoadFactor : Real read fMaxLoadFactor;

    {** A nil cursor. }
    property NilCursor: THashMapCursor read fNilCursor;

    property OnHashKey : THashKey read fOnHashKey write SetOnHashKey;
    property OnItemToString : TItemToString read fOnItemToString write SetOnItemToString;
    property OnKeysEqual : TKeysEqual read fOnKeysEqual write SetOnKeysEqual;
    property OnKeyToString : TKeyToString read fOnKeyToString write SetOnKeyToString;

    {** Number of items. }
    property Size : Integer read fSize;
  end;

  TAbstractHashSet = class(TContainer)
  end;

  { THashSetCursor }

  THashSetCursor = object
  strict private
    fHashSet : TAbstractHashSet;
    fPos : THashMapCursor;

  public
    {** Check if the cursors designate the same item. }
    function Equals(const Cursor: THashSetCursor) : Boolean;

    {** Check if the cursors designate an item. }
    function HasItem: Boolean; inline;

    {** Constructor. }
    constructor Init(HashSet : TAbstractHashSet; const APos: THashMapCursor);

    {** Returns true if the cursor designates the first element. }
    function IsFirst: Boolean; inline;

    {** Returns true if the cursor designates the last element. }
    function IsLast: Boolean; inline;

    {** Equivalent to (not HasItem). }
    function IsNil: Boolean; inline;

    {** If cursor is nil then do nothing, else if is last then cursor becomes nil
      cursor, otherwise move cursor to the next item.  }
    procedure MoveNext;

    property HashSet : TAbstractHashSet read fHashSet;

    {** Designated entry. }
    property Pos : THashMapCursor read fPos;
  end;

  { TGenHashSet }

  generic TGenHashSet<_TItem_> = class(TAbstractHashSet)
  strict private type
    TItemEquals = function (const A, B: _TItem_) : Boolean of object;
    TItemToString = function (const Item: _TItem_) : String of object;
    THashItem = function (const Item: _TItem_) : Integer of object;
    TMap = specialize TGenHashMap<_TItem_, Integer>;
    TEnumerator = specialize TGenEnumerator<_TItem_, THashSetCursor>;

  strict private
    fMap : TMap;
    fNilCursor : THashSetCursor;

    function EnumeratorGet(const Pos: THashSetCursor) : _TItem_;
    function EnumeratorNext(var Pos: THashSetCursor) : Boolean;
    procedure ExchangeContent(ASet: TGenHashSet);
    function GetItemToString: TItemToString;
    function GetOnHashItem: THashItem;
    function GetOnItemsEqual: TItemEquals;
    function GetSize: Integer; inline;
    procedure SetOnHashItem(AValue: THashItem);
    procedure SetOnItemsEqual(AValue: TItemEquals);
    procedure SetOnItemToString(AValue: TItemToString);

  public
    {** Removes all the items from the container. }
    procedure Clear;

    {** Returns true if the container contains Item. }
    function Contains(const Item: _TItem_) : Boolean;

    {** Creates an empty hash set and sets his capacity to InitialCapacity. }
    constructor Create(InitialCapacity: Integer = 16);

    {** Creates an empty hash set and sets his load factor to LoadFact. }
    constructor Create(LoadFact: Real);

    {** Creates an empty hash set and sets his capacity to InitialCapacity and
      his load factor to LoadFact. }
    constructor Create(InitialCapacity: Integer; LoadFact: Real);

    function DefaultItemsEqual(const A, B: _TItem_) : Boolean; virtual;
    function DefaultItemToString(const Item: _TItem_) : String; virtual;
    function DefaultHashItem(const Item: _TItem_) : Integer; virtual;

    {** Checks if Item is present in the container. If a match is found, removes
      the element from the set. Otherwise, raises an exception. }
    procedure Delete(const Item: _TItem_);

    {** Deletes the item designated by Position. }
    procedure DeleteAt(const Position: THashSetCursor);

    destructor Destroy; override;

    {** Clears Self and then adds to Self all the items of Left that are not
      present in Right. }
    procedure Difference(Left, Right: TGenHashSet);

    {** Checks if Item is present in the container. If a match is found, removes
      the item from the set. }
    procedure Exclude(const Item: _TItem_);

    {** Excludes all the items of ASet. }
    procedure ExcludeAll(ASet: TGenHashSet);

    {** Returns a cursor that designates the first element of the container or
      NilCursor if the container is empty. }
    function First: THashSetCursor;

    function GetEnumerator : TEnumerator;

    function GetItemAt(const Position: THashSetCursor): _TItem_;

    {** Checks if Item is present in the container. If no match is found, inserts
      the item into the set.}
    procedure Include(const Item: _TItem_);

    {** Includes all the items of ASet. }
    procedure IncludeAll(ASet: TGenHashSet);

    {** Checks if Item is present in the container. If no match is found, inserts
      the item into the set. Otherwise, raises an exception. }
    procedure Insert(const Item: _TItem_);

    {** Checks if Item is present in the container. If no match is found, inserts
      the item into the set and sets Inserted to true. Otherwise, sets Inserted
      to false. }
    procedure Insert(const Item: _TItem_; out Inserted: Boolean);

    {** Clears Self and then adds to Self all the items of Left that are present
      in Right. }
    procedure Intersection(Left, Right: TGenHashSet);

    {** Returns true if the container is empty. }
    function IsEmpty: Boolean; inline;

    {** Returns true if all the items in Self are present in OfSet. }
    function IsSubset(OfSet: TGenHashSet) : Boolean;

    {** Returns true if at least one item of Self is present in ASet. }
    function Overlaps(ASet: TGenHashSet) : Boolean;

    procedure ReadItemAt(const Position: THashSetCursor; out Value: _TItem_);

    {** Clears Self and then adds to Self all the items of Left that are not
      present in Right all the items of Right that are not present in Left. }
    procedure SymmetricDifference(Left, Right: TGenHashSet);

    {** Return a string representation for the container. }
    function ToString : String; override;

    {** Clears Self and then adds to Self all the items of Left and all the items
      of Right. }
    procedure Union(Left, Right: TGenHashSet);

    {** Provides access to the items in the container. }
    property Items[const Position: THashSetCursor] : _TItem_ read GetItemAt;
      default;

    {** A nil cursor. }
    property NilCursor: THashSetCursor read fNilCursor;

    property OnItemsEqual : TItemEquals read GetOnItemsEqual write SetOnItemsEqual;
    property OnItemToString : TItemToString read GetItemToString write SetOnItemToString;
    property OnHashItem : THashItem read GetOnHashItem write SetOnHashItem;

    property Size : Integer read GetSize;
  end;

  TAbstractTreeMap = class;

  { TTreeMapCursor }

  TTreeMapCursor = object
  strict private
    fTreeMap : TAbstractTreeMap;
    fEntry : Pointer;

  public
    {** Check if the cursors designate the same item. }
    function Equals(const Cursor: TTreeMapCursor) : Boolean; inline;

    {** Check if the cursors designate an item. }
    function HasItem: Boolean; inline;

    {** Constructor. }
    constructor Init(Map : TAbstractTreeMap; AnEntry: Pointer = nil);

    {** Returns true if the cursor designates the first element. }
    function IsFirst: Boolean; inline;

    {** Returns true if the cursor designates the last element. }
    function IsLast: Boolean; inline;

    {** Equivalent to (not HasItem). }
    function IsNil: Boolean; inline;

    {** If cursor is nil then do nothing, else if cursor is last then cursor
      becomes nil cursor, otherwise move cursor to the next item.  }
    procedure MoveNext; inline;

    {** If cursor is nil then do nothing, else if cursor is first then cursor
      becomes nil cursor, otherwise move cursor to the previous item.  }
    procedure MovePrevious; inline;

    property TreeMap : TAbstractTreeMap read fTreeMap;

    {** Designated entry. }
    property Entry : Pointer read fEntry write fEntry;
  end;

  TAbstractTreeMap = class(TContainer)
  protected
    function CursorIsFirst(const Cursor: TTreeMapCursor) : Boolean; virtual; abstract;
    function CursorIsLast(const Cursor: TTreeMapCursor) : Boolean; virtual; abstract;
    procedure CursorMoveNext(const Cursor: TTreeMapCursor); virtual; abstract;
    procedure CursorMovePrev(const Cursor: TTreeMapCursor); virtual; abstract;
  end;

  { TGenTreeMap }

  generic TGenTreeMap<_TKey_, _TItem_> = class(TAbstractTreeMap)
  public type
    TCompareKeys = function (const A, B: _TKey_) : Integer of object;
    TItemToString = function (const Item: _TItem_) : String of object;
    TKeyToString = function (const Key: _TKey_) : String of object;
    TEnumerator = specialize TGenEnumerator<_TItem_, TTreeMapCursor>;

    PItem = ^_TItem_;

  private type
    TColor = (cBlack, cRed);

    PEntry = ^TEntry;

    TEntry = record
      Color : TColor;
      Key : _TKey_;
      Left : PEntry;
      Parent : PEntry;
      Right : PEntry;
      Value : _TItem_;
    end;
  strict private
    fNilCursor : TTreeMapCursor;
    fOnCompareKeys: TCompareKeys;
    fOnItemToString: TItemToString;
    fOnKeyToString: TKeyToString;
    fSize : Integer;
    fRoot : PEntry;

    function ColorOf(E: PEntry) : TColor; inline;
    procedure DeleteEntry(E: PEntry);
    procedure DeleteTree(E: PEntry);
    function EnumeratorGet(const Pos: TTreeMapCursor) : _TItem_;
    function EnumeratorNext(var Pos: TTreeMapCursor) : Boolean;
    procedure RepairAfterDelete(E: PEntry);
    procedure RepairAfterInsert(E: PEntry);
    function GetCeilingEntry(const Key: _TKey_) : PEntry;
    function GetEntry(const Key: _TKey_) : PEntry;
    function GetFirstEntry : PEntry;
    function GetFloorEntry(const Key: _TKey_) : PEntry;
    function GetLastEntry : PEntry;
    function LeftOf(E: PEntry) : PEntry; inline;
    function NewEntry(AParent: PEntry; const AKey: _TKey_;
      const AValue: _TItem_) : PEntry;
    function ParentOf(E: PEntry) : PEntry; inline;
    function Predecessor(E: PEntry) : PEntry;
    function RightOf(E: PEntry) : PEntry; inline;
    procedure RotateLeft(E: PEntry);
    procedure RotateRight(E: PEntry);
    procedure SetColor(E: PEntry; Color: TColor);
    procedure SetOnCompareKeys(AValue: TCompareKeys);
    procedure SetOnItemToString(AValue: TItemToString);
    procedure SetOnKeyToString(AValue: TKeyToString);
    function Successor(E: PEntry) : PEntry;

  protected
    function CursorIsFirst(const Cursor: TTreeMapCursor) : Boolean; override;
    function CursorIsLast(const Cursor: TTreeMapCursor) : Boolean;  override;
    procedure CursorMoveNext(const Cursor: TTreeMapCursor); override;
    procedure CursorMovePrev(const Cursor: TTreeMapCursor); override;

  public
    {** Searches for the first entry whose key is not less than Key. If such an
      entry is found, a cursor that designates it is returned. Otherwise
      NilCursor is returned. }
    function Ceiling(const Key: _TKey_) : TTreeMapCursor;

    {** Removes all the items from the container. }
    procedure Clear;

    {** Returns true if the container contains Key. }
    function Contains(const Key : _TKey_) : Boolean;

    {** Creates an empty tree map. }
    constructor Create;

    function DefaultCompareKeys(const A, B: _TKey_) : Integer; virtual;
    function DefaultItemToString(const Item: _TItem_) : String; virtual;
    function DefaultKeyToString(const Key: _TKey_) : String; virtual;

    {** Checks if an item with the key to Key is present. If a match is found,
      removes the item from the map. Otherwise raise an exception. }
    procedure Delete(const Key: _TKey_);

    {** Deletes the item designated by Position. }
    procedure DeleteAt(const Position: TTreeMapCursor);

    {** Deletes the first item. }
    procedure DeleteFirst;

    {** Deletes the last item. }
    procedure DeleteLast;

    {** Destroys the container. }
    destructor Destroy; override;

    {** Checks if an item with the key Key is present. If a match is found,
      removes the item from the map. }
    procedure Exclude(const Key : _TKey_);

    {** Checks if an item associated with Key is present. If a match is found, a
      cursor designating the matching item is returned. Otherwise, NilCursor
      is returned. }
    function Find(const Key : _TKey_) : TTreeMapCursor;

    {** Returns a cursor that designates the first element of the container or
      NilCursor if the container is empty. }
    function First: TTreeMapCursor;

    {** Returns the first Item. }
    function FirstItem: _TItem_;

    {** If the map is empty raises an exception. Otherwise, returns the smallest
      Key. }
    function FirstKey: _TKey_;

    {** Searches for the last entry whose key is not greater than Key. If such
      an entry is found, a cursor that designates it is returned. Otherwise
      NilCursor is returned. }
    function Floor(const Key: _TKey_) : TTreeMapCursor;

    function GetEnumerator : TEnumerator;

    function GetItem(const Key: _TKey_): _TItem_;

    function GetItemAt(const Position: TTreeMapCursor): _TItem_;

    function GetKeyAt(const Position : TTreeMapCursor) : _TKey_;

    {** Inserts Key and Value into the map. If an entry with the same Key is
      already in the map, then the old value is replaced. }
    procedure Include(const Key : _TKey_; const Value: _TItem_);

    {** Inserts Key and Value into the map. If an entry with the same Key is
      already in the map, then an exception is raised. }
    procedure Insert(const Key : _TKey_; const Value: _TItem_);

    {** If an entry with the same Key is already in the map, then Inserted is
      set to false. Otherwise, Insert inserts Key and Value into the map and
      sets Inserted to true. }
    procedure Insert(const Key : _TKey_; const Value: _TItem_;
      out Inserted: Boolean);

    {** Returns true if the container is empty. }
    function IsEmpty: Boolean; inline;

    {** Returns a cursor that designates the last element of the container or
      NilCursor if the container is empty. }
    function Last: TTreeMapCursor;

    {** Returns the last Item. }
    function LastItem: _TItem_;

    {** If the map is empty raises an exception. Otherwise, returns the greatest
      Key. }
    function LastKey: _TKey_;

    procedure ReadFirstItem(out Value : _TItem_); inline;

    procedure ReadFirstKey(out Key : _TKey_); inline;

    procedure ReadItem(const Key: _TKey_; out Value: _TItem_);

    procedure ReadItemAt(const Position: TTreeMapCursor; out Value: _TItem_);

    procedure ReadKeyAt(const Position : TTreeMapCursor; out Key: _TKey_);

    procedure ReadLastItem(out Value : _TItem_); inline;

    procedure ReadLastKey(out Key : _TKey_); inline;

    {** Checks if an entry with the key Key is present. If a match is found,
      assigns Key and Value to the matching entry. Otherwise, an exception is
      raised. }
    procedure Replace(const Key : _TKey_; const Value: _TItem_);

    procedure SetItemAt(const Position: TTreeMapCursor; Value: _TItem_);

    {** Return a string representation for the container. }
    function ToString : String; override;

    {** Provides access to the items in the container. }
    property ItemAt[const Position: TTreeMapCursor] : _TItem_ read GetItemAt
      write SetItemAt;

    {** Provides access to the items in the container. }
    property Items[const Key: _TKey_] : _TItem_ read GetItem write Include;
      default;

    {** Provides access to the keys in the container. }
    property Keys[const Position: TTreeMapCursor] : _TKey_ read GetKeyAt;

    {** A nil cursor. }
    property NilCursor: TTreeMapCursor read fNilCursor;

    property OnCompareKeys : TCompareKeys read fOnCompareKeys write SetOnCompareKeys;
    property OnItemToString : TItemToString read fOnItemToString write SetOnItemToString;
    property OnKeyToString : TKeyToString read fOnKeyToString write SetOnKeyToString;

    {** Number of items. }
    property Size : Integer read fSize;
  end;

  TAbstractTreeSet = class(TContainer)
  end;

  TTreeSetCursor = object
  strict private
    fTreeSet : TAbstractTreeSet;
    fPos : TTreeMapCursor;

  public
    {** Check if the cursors designate the same item. }
    function Equals(const Cursor: TTreeSetCursor) : Boolean;

    {** Check if the cursors designate an item. }
    function HasItem: Boolean; inline;

    {** Constructor. }
    constructor Init(TreeSet : TAbstractTreeSet; const APos: TTreeMapCursor);

    {** Returns true if the cursor designates the first element. }
    function IsFirst: Boolean; inline;

    {** Returns true if the cursor designates the last element. }
    function IsLast: Boolean; inline;

    {** Equivalent to (not HasItem). }
    function IsNil: Boolean; inline;

    {** If cursor is nil then do nothing, else if cursor is last then cursor
      becomes nil cursor, otherwise move cursor to the next item.  }
    procedure MoveNext;

    {** If cursor is nil then do nothing, else if cursor is first then cursor
      becomes nil cursor, otherwise move cursor to the previous item.  }
    procedure MovePrevious;

    property TreeSet : TAbstractTreeSet read fTreeSet;

    {** Designated entry. }
    property Pos : TTreeMapCursor read fPos;
  end;

  { TGenTreeSet }

  generic TGenTreeSet<_TItem_> = class(TAbstractTreeSet)
  public type
    TCompareItems = function (const A, B: _TItem_) : Integer of object;
    TItemToString = function (const Item: _TItem_) : String of object;
    TEnumerator = specialize TGenEnumerator<_TItem_, TTreeSetCursor>;

  private type
    TMap = specialize TGenTreeMap<_TItem_, Integer>;

  private
    fMap : TMap;
    fNilCursor : TTreeSetCursor;

    function EnumeratorGet(const Pos: TTreeSetCursor) : _TItem_;
    function EnumeratorNext(var Pos: TTreeSetCursor) : Boolean;
    procedure ExchangeContent(ASet: TGenTreeSet);
    function GetOnCompareItems: TCompareItems;
    function GetOnItemToString: TItemToString;
    function GetSize: Integer; inline;
    procedure SetOnCompareItems(AValue: TCompareItems);
    procedure SetOnItemToString(AValue: TItemToString);

  public
    {** Searches for the first item which is not less than Item. If such an item
      is found, a cursor that designates it is returned. Otherwise NilCursor is
      returned. }
    function Ceiling(const Item: _TItem_) : TTreeSetCursor;

    {** Removes all the items from the container. }
    procedure Clear;

    {** Returns true if the container contains Item. }
    function Contains(const Item: _TItem_) : Boolean;

    {** Creates an empty tree set. }
    constructor Create;

    function DefaultCompareItems(const A, B: _TItem_) : Integer; virtual;
    function DefaultItemToString(const Item: _TItem_) : String; virtual;

    {** Checks if Item is present in the container. If a match is found, removes
      the element from the set. Otherwise, raises an exception. }
    procedure Delete(const Item: _TItem_);

    {** Deletes the item designated by Position. }
    procedure DeleteAt(const Position: TTreeSetCursor);

    {** Deletes the first item. }
    procedure DeleteFirst;

    {** Deletes the last item. }
    procedure DeleteLast;

    destructor Destroy; override;

    {** Clears Self and then adds to Self all the items of Left that are not
      present in Right. }
    procedure Difference(Left, Right: TGenTreeSet);

    {** Checks if Item is present in the container. If a match is found, removes
      the item from the set. }
    procedure Exclude(const Item: _TItem_);

    {** Excludes all the items of ASet. }
    procedure ExcludeAll(ASet: TGenTreeSet);

    {** Returns a cursor that designates the first element of the container or
      NilCursor if the container is empty. }
    function First: TTreeSetCursor;

    {** Returns the first Item. }
    function FirstItem: _TItem_;

    {** Searches for the last item which is not greater than Item. If such an
      item is found, a cursor that designates it is returned. Otherwise
      NilCursor is returned. }
    function Floor(const Item: _TItem_) : TTreeSetCursor;

    function GetEnumerator : TEnumerator;

    function GetItemAt(const Position: TTreeSetCursor): _TItem_;

    {** Checks if Item is present in the container. If no match is found, inserts
      the item into the set.}
    procedure Include(const Item: _TItem_);

    {** Includes all the items of ASet. }
    procedure IncludeAll(ASet: TGenTreeSet);

    {** Checks if Item is present in the container. If no match is found, inserts
      the item into the set. Otherwise, raises an exception. }
    procedure Insert(const Item: _TItem_);

    {** Checks if Item is present in the container. If no match is found, inserts
      the item into the set and sets Inserted to true. Otherwise, sets Inserted
      to false. }
    procedure Insert(const Item: _TItem_; out Inserted: Boolean);

    {** Clears Self and then adds to Self all the items of Left that are present
      in Right. }
    procedure Intersection(Left, Right: TGenTreeSet);

    {** Returns true if the set is empty. }
    function IsEmpty: Boolean; inline;

    {** Returns true if all the items in Self are present in OfSet. }
    function IsSubset(OfSet: TGenTreeSet) : Boolean;

    {** Returns a cursor that designates the last element of the container
      or NilCursor if the container is empty. }
    function Last: TTreeSetCursor;

    {** Returns the last Item. }
    function LastItem: _TItem_;

    {** Returns true if at least one item of Self is present in ASet. }
    function Overlaps(ASet: TGenTreeSet) : Boolean;

    procedure ReadFirstItem(out Value : _TItem_); inline;

    procedure ReadItemAt(const Position: TTreeSetCursor; out Value: _TItem_);

    procedure ReadLastItem(out Value : _TItem_); inline;

    {** Clears Self and then adds to Self all the items of Left that are not
      present in Right all the items of Right that are not present in Left. }
    procedure SymmetricDifference(Left, Right: TGenTreeSet);

    {** Return a string representation for the container. }
    function ToString : String; override;

    {** Clears Self and then adds to Self all the items of Left and all the
      items of Right. }
    procedure Union(Left, Right: TGenTreeSet);

    {** Provides access to the items in the container. }
    property Items[const Position: TTreeSetCursor] : _TItem_ read GetItemAt; default;

    {** A nil cursor. }
    property NilCursor: TTreeSetCursor read fNilCursor;

    property OnCompareItems : TCompareItems read GetOnCompareItems write SetOnCompareItems;

    property OnItemToString : TItemToString read GetOnItemToString write SetOnItemToString;

    {** Number of items. }
    property Size : Integer read GetSize;
  end;


  { TBitSet }

  {** Class to store collections of bits. }

  TBitSet = class(TContainer)
  private
    fBits : array of Byte;
    fExtraMask : Byte;
    fLen : Integer;
    fSize : Integer;

    procedure ClearExtraBits;
  public
    {** Performs a logical AND on the bits in the bitset with the bits of
      BitSet. }
    procedure AndBits(BitSet : TBitSet);

    {** Returns @true if all the bits of the bitset are set, and @false
      otherwise. }
    function All : Boolean;

    {** Returns @true if any of the bits in the bitset is set, and @false
      otherwise. }
    function Any : Boolean;

    {** Returns the number of bits in the bitset that are set. }
    function Cardinality : Integer;

    {** Sets to @false the bit at position Index.}
    procedure Clear(Index: Integer);

    {** Sets to @false all bits in the bitset.}
    procedure ClearAll;

    constructor Create(Size: Integer);

    constructor Create(Size: Integer; Value: QWord);

    constructor Create(Size: Integer; const Value: String);

    constructor Create(Value: TBitSet);

    constructor Create(Size: Integer; Value: TBitSet);

    procedure Debug;

    {** Returns @true only if the bitset and Value are of the same size and have
      exactly the same set of bits set to @true.}
    function Equals(Obj: TObject) : Boolean; override;

    {** Flips the bit at position Index. }
    procedure Flip(Index: Integer);

    {** Flips all bits in the bitset. }
    procedure FlipAll;

    procedure FlipFast(Index: Integer); inline;

    procedure Initialize(Value: Int64);

    procedure Initialize(const Value : String);

    procedure Initialize(Value : TBitSet);

    {** Returns @true if none of the bits in the bitset is set to @true, and
      @false otherwise.}
    function None : Boolean;

    procedure NotBits(BitSet : TBitSet);

    {** Performs a logical OR on the bits in the bitset with the bits of
      BitSet. }
    procedure OrBits(BitSet : TBitSet);

    {** Sets to @true all bits in the bitset.}
    procedure SetAll;

    {** Sets to Value the bit at position Index.}
    procedure SetBit(Index : Integer; Value: Boolean);

    procedure SetBitFast(Index : Integer; Value: Boolean); inline;

    {** Sets to @true the bit at position Index.}
    procedure SetOn(Index: Integer);

    procedure ShiftLeft(Count : Integer = 1);

    procedure ShiftRight(Count : Integer = 1);

    {** Returns @true if the bit at position Index is set, and @false
      otherwise.}
    function Test(Index: Integer) : Boolean;

    function TestFast(Index : Integer): Boolean; inline;

    function ToInteger: Integer;

    function ToInt64: Int64;

    function ToQWord: QWord;

    function ToString: String; override;

    {** Performs a logical XOR on the bits in the bitset with the bits of
      BitSet. }
    procedure XorBits(BitSet : TBitSet);

    property Bits[Index : Integer] : Boolean read TestFast
      write SetBitFast; default;

    property Size : Integer read fSize;
  end;

function HashData(Data : PByte; DataSize: Integer) : Integer;
function HashString(const Str: String) : Integer;

implementation

uses Math;

const
  S_BitSetsAreIncompatible = 'bit sets are incompatible';
  S_ContainerEmpty = 'container is empty';
  S_CursorIsNil = 'cursor is nil';
  S_CursorDenotesWrongContainer = 'cursor denotes wrong container';
  S_IndexOutOfRange = 'index out of range';
  S_InvalidBitSetSize = 'invalid bit set size';
  S_InvalidBinaryValue = 'invalid binary value';
  S_ItemNotInSet = 'item not in set';
  S_ItemAlreadyInSet = 'item already in set';
  S_KeyNotInMap = 'key not in map';
  S_KeyAlreadyInMap = 'key already in map';
  S_MethodNotRedefined = 'method not redefined';

  SBox : array [Byte] of LongWord = ( $F53E1837, $5F14C86B, $9EE3964C,
    $FA796D53, $32223FC3, $4D82BC98, $A0C7FA62, $63E2C982, $24994A5B, $1ECE7BEE,
    $292B38EF, $D5CD4E56, $514F4303, $7BE12B83, $7192F195, $82DC7300, $084380B4,
    $480B55D3, $5F430471, $13F75991, $3F9CF22C, $2FE0907A, $FD8E1E69, $7B1D5DE8,
    $D575A85C, $AD01C50A, $7EE00737, $3CE981E8, $0E447EFA, $23089DD6, $B59F149F,
    $13600EC7, $E802C8E6, $670921E4, $7207EFF0, $E74761B0, $69035234, $BFA40F19,
    $F63651A0, $29E64C26, $1F98CCA7, $D957007E, $E71DDC75, $3E729595, $7580B7CC,
    $D7FAF60B, $92484323, $A44113EB, $E4CBDE08, $346827C9, $3CF32AFA, $0B29BCF1,
    $6E29F7DF, $B01E71CB, $3BFBC0D1, $62EDC5B8, $B7DE789A, $A4748EC9, $E17A4C4F,
    $67E5BD03, $F3B33D1A, $97D8D3E9, $09121BC0, $347B2D2C, $79A1913C, $504172DE,
    $7F1F8483, $13AC3CF6, $7A2094DB, $C778FA12, $ADF7469F, $21786B7B, $71A445D0,
    $A8896C1B, $656F62FB, $83A059B3, $972DFE6E, $4122000C, $97D9DA19, $17D5947B,
    $B1AFFD0C, $6EF83B97, $AF7F780B, $4613138A, $7C3E73A6, $CF15E03D, $41576322,
    $672DF292, $B658588D, $33EBEFA9, $938CBF06, $06B67381, $07F192C6, $2BDA5855,
    $348EE0E8, $19DBB6E3, $3222184B, $B69D5DBA, $7E760B88, $AF4D8154, $007A51AD,
    $35112500, $C9CD2D7D, $4F4FB761, $694772E3, $694C8351, $4A7E3AF5, $67D65CE1,
    $9287DE92, $2518DB3C, $8CB4EC06, $D154D38F, $E19A26BB, $295EE439, $C50A1104,
    $2153C6A7, $82366656, $0713BC2F, $6462215A, $21D9BFCE, $BA8EACE6, $AE2DF4C1,
    $2A8D5E80, $3F7E52D1, $29359399, $FEA1D19C, $18879313, $455AFA81, $FADFE838,
    $62609838, $D1028839, $0736E92F, $3BCA22A3, $1485B08A, $2DA7900B, $852C156D,
    $E8F24803, $00078472, $13F0D332, $2ACFD0CF, $5F747F5C, $87BB1E2F, $A7EFCB63,
    $23F432F0, $E6CE7C5C, $1F954EF6, $B609C91B, $3B4571BF, $EED17DC0, $E556CDA0,
    $A7846A8D, $FF105F94, $52B7CCDE, $0E33E801, $664455EA, $F2C70414, $73E7B486,
    $8F830661, $8B59E826, $BB8AEDCA, $F3D70AB9, $D739F2B9, $4A04C34A, $88D0F089,
    $E02191A2, $D89D9C78, $192C2749, $FC43A78F, $0AAC88CB, $9438D42D, $9E280F7A,
    $36063802, $38E8D018, $1C42A9CB, $92AAFF6C, $A24820C5, $007F077F, $CE5BC543,
    $69668D58, $10D6FF74, $BE00F621, $21300BBE, $2E9E8F46, $5ACEA629, $FA1F86C7,
    $52F206B8, $3EDF1A75, $6DA8D843, $CF719928, $73E3891F, $B4B95DD6, $B2A42D27,
    $EDA20BBF, $1A58DBDF, $A449AD03, $6DDEF22B, $900531E6, $3D3BFF35, $5B24ABA2,
    $472B3E4C, $387F2D75, $4D8DBA36, $71CB5641, $E3473F3F, $F6CD4B7F, $BF7D1428,
    $344B64D0, $C5CDFCB6, $FE2E0182, $2C37A673, $DE4EB7A3, $63FDC933, $01DC4063,
    $611F3571, $D167BFAF, $4496596F, $3DEE0689, $D8704910, $7052A114, $068C9EC5,
    $75D0E766, $4D54CC20, $B44ECDE2, $4ABC653E, $2C550A21, $1A52C0DB, $CFED03D0,
    $119BAFE2, $876A6133, $BC232088, $435BA1B2, $AE99BBFA, $BB4F08E4, $A62B5F49,
    $1DA4B695, $336B84DE, $DC813D31, $00C134FB, $397A98E6, $151F0E64, $D9EB3E69,
    $D3C7DF60, $D2F2C336, $2DDD067B, $BD122835, $B0B3BD3A, $B0D54E46, $8641F1E4,
    $A0B38F96, $51D39199, $37A6AD75, $DF84EE41, $3C034CBA, $ACDA62FC, $11923B8B,
    $45EF170A);

  Card : array [Byte] of Byte = (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, 1, 2, 2, 3, 2, 3, 3, 4, 2,
    3, 3, 4, 3, 4, 4, 5, 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, 1, 2,
    2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4,
    5, 4, 5, 5, 6, 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, 3, 4, 4, 5,
    4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3,
    4, 4, 5, 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, 2, 3, 3, 4, 3, 4,
    4, 5, 3, 4, 4, 5, 4, 5, 5, 6, 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6,
    7, 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, 3, 4, 4, 5, 4, 5, 5, 6,
    4, 5, 5, 6, 5, 6, 6, 7, 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, 4,
    5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

{--- HashData ---}
{$PUSH}
{$O-}{$R-}{$Q-}
function HashData(Data : PByte; DataSize: Integer) : Integer; 
var
  I : Integer;
begin
  Result := 0;
  for I := 1 to DataSize do
  begin
    Result := Result xor Integer(SBox[Data^]);
    Result := Result * 3;
    Inc(Data);
  end;
end;
{$POP}

{--- HashString ---}
function HashString(const Str: String): Integer;
begin
  if Str = '' then
    Result := 0
  else
    Result := HashData(@Str[1], Length(Str));
end;

{===============}
{=== TBitSet ===}
{===============}

{$push}
{$rangechecks off}
{$overflowchecks off}

{--- TBitSet.ClearExtraBits ---}
procedure TBitSet.ClearExtraBits;
begin
  if fExtraMask <> High(Byte) then
    fBits[fLen - 1] := fBits[fLen - 1] and fExtraMask;
end;

{--- TBitSet.AndBits ---}
procedure TBitSet.AndBits(BitSet: TBitSet);
var
  I: Integer;
begin
  if BitSet.fSize <> fSize then
    RaiseError(S_BitSetsAreIncompatible);

  for I := 0 to fLen - 1 do
    fBits[I] := fBits[I] and BitSet.fBits[I];

  ClearExtraBits;
end;

{--- TBitSet.All ---}
function TBitSet.All: Boolean;
var
  I: Integer;
begin
  for I := 0 to fLen - 2 do
    if fBits[I] <> High(Byte) then
    begin
      Result := false;
      Exit;
    end;

  Result := (fBits[fLen - 1] = fExtraMask);
end;

{--- TBitSet.Any ---}
function TBitSet.Any: Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to fLen - 1 do
    if fBits[I] <> 0 then
    begin
      Result := true;
      Break;
    end;
end;

{--- TBitSet.Cardinality ---}
function TBitSet.Cardinality: Integer;
var
  I : Integer;
begin
  Result := 0;

  for I := 0 to fLen - 2 do
    Result := Result + Card[fBits[I]];
  Result := Result + Card[(fBits[fLen - 1] and fExtraMask)];
end;

{--- TBitSet.Clear ---}
procedure TBitSet.Clear(Index: Integer);
begin
  if (Index < 0) or (Index >= fSize) then
    RaiseIndexOutOfRange;
  SetBitFast(Index, false);
end;

{--- TBitSet.ClearAll ---}
procedure TBitSet.ClearAll;
var
  I: Integer;
begin
  for I := Low(fBits) to High(fBits) do
    fBits[I] := 0;
end;

{--- TBitSet.Create ---}
constructor TBitSet.Create(Size: Integer);
var
  ArraySize, I : Integer;
begin
  if Size <= 0 then
    RaiseError(S_InvalidBitSetSize);

  fSize := Size;
  fLen := (fSize + (SizeOf(Byte) * 8) - 1) div (SizeOf(Byte) * 8);

  SetLength(fBits, fLen);

  ArraySize := fLen * SizeOf(Byte) * 8;
  if ArraySize = Size then
    fExtraMask := High(Byte)
  else
  begin
    fExtraMask := 1;
    for I := 2 to SizeOf(Byte) * 8 - (ArraySize - Size) do
      fExtraMask := (fExtraMask shl 1) or 1;
  end;
end;

{--- TBitSet.Create ---}
constructor TBitSet.Create(Size: Integer; Value: QWord);
begin
  Create(Size);
  Initialize(Value);
end;

{--- TBitSet.Create ---}
constructor TBitSet.Create(Size: Integer; const Value: String);
begin
  Create(Size);
  Initialize(Value);
end;

{--- TBitSet.Create ---}
constructor TBitSet.Create(Value: TBitSet);
begin
  Create(Value.fSize, Value);
end;

{--- TBitSet.Create ---}
constructor TBitSet.Create(Size: Integer; Value: TBitSet);
var
  I, IMax: Integer;
begin
  Create(Size);

  IMax := Min(Size - 1, Value.fSize - 1);
  for I := 0 to IMax do
    if Value.TestFast(I) then
      SetBitFast(I, true);
end;

{--- TBitSet.Debug ---}
procedure TBitSet.Debug;
var
  I: Integer;
begin
  Write('TBitSet@', HexStr(Self), ' : fLen=', fLen, ' fSize=', fSize);
  WriteLn(' fExtraMask=', BinStr(fExtraMask, SizeOf(fExtraMask) * 8));
  Write('fBits=[');
  for I := Low(fBits) to High(fBits) do
    Write(fBits[I], ' ');
  WriteLn(']');
end;

{--- TBitSet.Equals ---}
function TBitSet.Equals(Obj: TObject): Boolean;
var
  I: Integer;
  Value : TBitSet;
begin
  if Obj is TBitSet then
  begin
    Value := Obj as TBitSet;
    if fSize <> Value.fSize then
      RaiseError(S_BitSetsAreIncompatible);

    Result := true;
    for I := Low(fBits) to High(fBits) do
      if fBits[I] <> Value.fBits[I] then
      begin
        Result := false;
        Exit;
      end;
  end
  else
    Result := false;
end;

{--- TBitSet.Flip ---}
procedure TBitSet.Flip(Index: Integer);
var
  Rank, NBit : Integer;
begin
  if (Index < 0) or (Index >= fSize) then
    RaiseIndexOutOfRange;

  Rank := Index div (SizeOf(Byte) * 8);
  NBit := Index mod (SizeOf(Byte) * 8);

  fBits[Rank] := fBits[Rank] xor (Byte(1) shl NBit);
end;

{--- TBitSet.FlipAll ---}
procedure TBitSet.FlipAll;
var
  I: Integer;
begin
  for I := Low(fBits) to High(fBits) do
    fBits[I] := not fBits[I];
  ClearExtraBits;
end;

{--- TBitSet.FlipFast ---}
procedure TBitSet.FlipFast(Index: Integer);
var
  Rank, NBit : Integer;
begin
  Rank := Index div (SizeOf(Byte) * 8);
  NBit := Index mod (SizeOf(Byte) * 8);

  fBits[Rank] := fBits[Rank] xor (Byte(1) shl NBit);
end;

{--- TBitSet.Initialize ---}
procedure TBitSet.Initialize(Value: Int64);
const
  NBits = SizeOf(Int64) * 8;
var
  I, IMax: Integer;
begin
  ClearAll;

  IMax := Min(NBits - 1, fSize - 1);
  for I := 0 to IMax do
  begin
    if (Value and 1) <> 0 then
      SetBitFast(I, true);
    Value := Value shr 1;
  end;
end;

{--- TBitSet.Initialize ---}
procedure TBitSet.Initialize(const Value: String);
var
  I, IMax, Len: Integer;
begin
  ClearAll;

  Len := Length(Value);
  IMax := Min(Len, fSize);
  for I := 1 to IMax do
  begin
    if Value[I] = '1' then
      SetBitFast(IMax - I, true)
    else if Value[I] <> '0' then
      RaiseError(S_InvalidBinaryValue);
  end;
end;

{--- TBitSet.Initialize ---}
procedure TBitSet.Initialize(Value: TBitSet);
var
  I, IMax : Integer;
begin
  ClearAll;
  IMax := Min(fSize - 1, Value.fSize - 1);
  for I := 0 to IMax do
    SetBitFast(I, Value.TestFast(I));
end;

{--- TBitSet.None ---}
function TBitSet.None: Boolean;
begin
  Result := not Any;
end;

{--- TBitSet.NotBits ---}
procedure TBitSet.NotBits(BitSet: TBitSet);
var
  I: Integer;
  B : Integer;
begin
  if BitSet.fSize <> fSize then
    RaiseError(S_BitSetsAreIncompatible);

  for I := 0 to fLen - 1 do
  begin
    B := fBits[I];
    fBits[I] := B and (B xor BitSet.fBits[I]);
  end;

  ClearExtraBits;
end;

{--- TBitSet.OrBits ---}
procedure TBitSet.OrBits(BitSet: TBitSet);
var
  I: Integer;
begin
  if BitSet.fSize <> fSize then
    RaiseError(S_BitSetsAreIncompatible);

  for I := 0 to fLen - 1 do
    fBits[I] := fBits[I] or BitSet.fBits[I];

  ClearExtraBits;
end;

{--- TBitSet.SetAll ---}
procedure TBitSet.SetAll;
var
  I: Integer;
begin
  for I := Low(fBits) to High(fBits) do
    fBits[I] := High(Byte);
  ClearExtraBits;
end;

{--- TBitSet.SetBit ---}
procedure TBitSet.SetBit(Index: Integer; Value: Boolean);
begin
  if (Index < 0) or (Index >= fSize) then
    RaiseIndexOutOfRange;
  SetBitFast(Index, Value);
end;

{--- TBitSet.SetBitFast ---}
procedure TBitSet.SetBitFast(Index : Integer; Value: Boolean);
var
  Rank, NBit : Integer;
  Mask : Byte;
begin
  Rank := Index div (SizeOf(Byte) * 8);
  NBit := Index mod (SizeOf(Byte) * 8);

  Mask := 1 shl NBit;

  if Value then
    fBits[Rank] := fBits[Rank] or Mask
  else
  begin
    Mask := not Mask;
    fBits[Rank] := fBits[Rank] and Mask;
  end;

end;

{--- TBitSet.SetOn ---}
procedure TBitSet.SetOn(Index: Integer);
begin
  if (Index < 0) or (Index >= fSize) then
    RaiseIndexOutOfRange;
  SetBitFast(Index, true);
end;

{--- TBitSet.ShiftLeft ---}
procedure TBitSet.ShiftLeft(Count: Integer);
var
  I: Integer;
begin
  if Count = 0 then
    Exit
  else if Count < 0 then
    ShiftRight(- Count)
  else if Count >= fSize then
    ClearAll
  else if Count mod 8 = 0 then
  begin
    Count := Count div 8;

    for I := fLen - Count - 1 downto 0 do
      fBits[I + Count] := fBits[I];

    for I := 0 to Count - 1 do
      fBits[I] := 0;

    ClearExtraBits;
  end
  else
  begin
    for I := fSize - Count - 1 downto 0  do
      SetBitFast(I + Count, TestFast(I));

    for I := 0 to Count - 1  do
      SetBitFast(I, false);
  end;
end;

{--- TBitSet.ShiftRight ---}
procedure TBitSet.ShiftRight(Count: Integer);
var
  I : Integer;
begin
  if Count = 0 then
    Exit
  else if Count < 0 then
    ShiftLeft(- Count)
  else if Count >= fSize then
    ClearAll
  else if Count mod 8 = 0 then
  begin
    Count := Count div 8;

    for I := Count to fLen - 1 do
      fBits[I - Count] := fBits[I];

    for I := fLen - 1 downto fLen - Count do
      fBits[I] := 0;

    ClearExtraBits;
  end
  else
  begin
    for I := Count to fSize - 1 do
      SetBitFast(I - Count, TestFast(I));

    for I := fSize - Count to fSize - 1 do
      SetBitFast(I, false);
  end;
end;

{--- TBitSet.Test ---}
function TBitSet.Test(Index: Integer): Boolean;
begin
  if (Index < 0) or (Index >= fSize) then
    RaiseIndexOutOfRange;
  Result := TestFast(Index);
end;

{--- TBitSet.TestFast ---}
function TBitSet.TestFast(Index : Integer): Boolean;
var
  Rank, NBit : Integer;
begin
  Rank := Index div (SizeOf(Byte) * 8);
  NBit := Index mod (SizeOf(Byte) * 8);

  Result := (fBits[Rank] and (1 shl NBit)) <> 0;
end;

{--- TBitSet.ToInteger ---}
function TBitSet.ToInteger: Integer;
var
  I, IMax : Integer;
begin
  Result := 0;

  IMax := Min(fSize - 1, SizeOf(Integer) * 8 - 1);
  for I := IMax downto 0 do
  begin
    Result := Result shl 1;
    if TestFast(I) then
      Result := Result or 1;
  end;
end;

{--- TBitSet.ToInt64 ---}
function TBitSet.ToInt64: Int64;
begin
  Result := Int64(ToQWord);
end;

{--- TBitSet.ToQWord ---}
function TBitSet.ToQWord: QWord;
var
  I, IMax : Integer;
begin
  Result := 0;

  IMax := Min(fSize - 1, SizeOf(QWord) * 8 - 1);
  for I := IMax downto 0 do
  begin
    Result := Result shl 1;
    if TestFast(I) then
      Result := Result or 1;
  end;
end;

{--- TBitSet.ToString ---}
function TBitSet.ToString: String;
var
  Bit : Char;
  I: Integer;
begin
  SetLength(Result, fSize);

  for I := 0 to fSize - 1 do
  begin
    if TestFast(I) then
      Bit := '1'
    else
      Bit := '0';

    Result[fSize - I] := Bit;
  end;
end;

{--- TBitSet.XorBits ---}
procedure TBitSet.XorBits(BitSet: TBitSet);
var
  I: Integer;
begin
  if BitSet.fSize <> fSize then
    RaiseError(S_BitSetsAreIncompatible);

  for I := 0 to fLen - 1 do
    fBits[I] := fBits[I] xor BitSet.fBits[I];

  ClearExtraBits;
end;

{$pop}

{======================}
{=== TGenEnumerator ===}
{======================}

{--- TGenEnumerator.GetCurrent ---}
function TGenEnumerator.GetCurrent: _TItem_;
begin
  Result := fGetter(fPos);
end;

{--- TGenEnumerator.Create ---}
constructor TGenEnumerator.Create(const Pos: _TPosition_; Mover: TMoveNext;
  Getter: TGetCurrent);
begin
  fPos := Pos;
  fMover := Mover;
  fGetter := Getter;
end;

{--- TGenEnumerator.MoveNext ---}
function TGenEnumerator.MoveNext: Boolean;
begin
  Result := fMover(fPos);
end;

{==================}
{=== TContainer ===}
{==================}

{--- TContainer.RaiseContainerEmpty ---}
procedure TContainer.RaiseContainerEmpty;
begin
  raise EContainerError.Create(S_ContainerEmpty);
end;

{--- TContainer.RaiseCursorDenotesWrongContainer ---}
procedure TContainer.RaiseCursorDenotesWrongContainer;
begin
  raise EContainerError.Create(S_CursorDenotesWrongContainer);
end;

{--- TContainer.RaiseCursorIsNil ---}
procedure TContainer.RaiseCursorIsNil;
begin
  raise EContainerError.Create(S_CursorIsNil);
end;

{--- TContainer.RaiseError ---}
procedure TContainer.RaiseError(const Msg: String);
begin
  raise EContainerError.Create(Msg);
end;

{--- TContainer.RaiseIndexOutOfRange ---}
procedure TContainer.RaiseIndexOutOfRange;
begin
  raise EContainerError.Create(S_IndexOutOfRange);
end;

{--- TContainer.RaiseItemAlreadyInSet ---}
procedure TContainer.RaiseItemAlreadyInSet;
begin
  raise EContainerError.Create(S_ItemAlreadyInSet);
end;

{--- TContainer.RaiseItemNotInSet ---}
procedure TContainer.RaiseItemNotInSet;
begin
  raise EContainerError.Create(S_ItemNotInSet);
end;

{--- TContainer.RaiseKeyAlreadyInMap ---}
procedure TContainer.RaiseKeyAlreadyInMap;
begin
  raise EContainerError.Create(S_KeyAlreadyInMap);
end;

{--- TContainer.RaiseKeyNotInMap ---}
procedure TContainer.RaiseKeyNotInMap;
begin
  raise EContainerError.Create(S_KeyNotInMap);
end;

{--- TContainer.RaiseMethodNotRedefined ---}
procedure TContainer.RaiseMethodNotRedefined;
begin
  raise EContainerError.Create(S_MethodNotRedefined);
end;

{--- TContainer.Unused ---}
{$PUSH}
{$HINTS OFF}
procedure TContainer.Unused(P: Pointer); inline;
begin
end;
{$POP}

{=======================}
{=== TAbstractVector ===}
{=======================}

{--- TAbstractVector.CheckIndex ---}
procedure TAbstractVector.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= fSize) then
    RaiseIndexOutOfRange;
end;

{--- TAbstractVector.CheckIndexForAdd ---}
procedure TAbstractVector.CheckIndexForAdd(Index: Integer);
begin
  if (Index < 0) or (Index > fSize) then
    RaiseIndexOutOfRange;
end;

{--- TAbstractVector.Clear ---}
procedure TAbstractVector.Clear;
begin
  Resize(0);
end;

{--- TAbstractVector.Delete ---}
procedure TAbstractVector.Delete(Position: Integer; Count: Integer);
var
  CountAtEnd: Integer;
begin
  CheckIndex(Position);

  if Position + Count > fSize then
    Count := fSize - Position;

  if Count > 0 then
  begin
    CountAtEnd := fSize - (Position + Count);
    if CountAtEnd > 0 then
      Move(Position + Count, Position, CountAtEnd);

    fSize := fSize - Count;
  end;
end;

{--- TAbstractVector.DeleteFirst ---}
procedure TAbstractVector.DeleteFirst(Count: Integer);
begin
  if Count > 0 then
    Delete(0, Count);
end;

{--- TAbstractVector.DeleteLast ---}
procedure TAbstractVector.DeleteLast(Count: Integer);
begin
  if Count > 0 then
    Resize(fSize - Count);
end;

{--- TAbstractVector.DeleteRange ---}
procedure TAbstractVector.DeleteRange(PosFrom, PosTo: Integer);
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  if PosTo >= PosFrom then
    Delete(PosFrom, PosTo - PosFrom + 1);
end;

{--- TAbstractVector.InsertSpace ---}
procedure TAbstractVector.InsertSpace(Position: Integer; Count: Integer);
begin
  CheckIndexForAdd(Position);
  InsertSpaceFast(Position, Count);
end;

{--- TAbstractVector.IsEmpty ---}
function TAbstractVector.IsEmpty: Boolean;
begin
  Result := (fSize = 0);
end;

{--- TAbstractVector.Reserve ---}
procedure TAbstractVector.Reserve(MinCapacity: Integer);
var
  NewCapacity : Integer;
begin
  if MinCapacity > Capacity then
  begin
    NewCapacity := (Capacity * 3) div 2;
    if NewCapacity < MinCapacity then
      NewCapacity := MinCapacity;
    SetCapacity(NewCapacity);
  end;
end;

{--- TAbstractVector.Resize ---}
procedure TAbstractVector.Resize(NewSize: Integer);
begin
  if NewSize > fSize then
    Reserve(NewSize);

  if NewSize < 0 then
    NewSize := 0;

  fSize := NewSize;
end;

{--- TAbstractVector.Reverse ---}
procedure TAbstractVector.Reverse;
begin
  if fSize > 1 then
    ReverseRange(0, fSize - 1);
end;

{--- TAbstractVector.ReverseRange ---}
procedure TAbstractVector.ReverseRange(PosFrom, PosTo: Integer);
var
  TmpIndex : Integer;
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  if PosTo < PosFrom then
  begin
    TmpIndex := PosFrom;
    PosFrom := PosTo;
    PosTo := TmpIndex;
  end;

  while PosFrom < PosTo do
  begin
    SwapFast(PosFrom, PosTo);
    Inc(PosFrom);
    Dec(PosTo);
  end;
end;

{--- TAbstractVector.Shuffle ---}
procedure TAbstractVector.Shuffle;
begin
   if fSize > 1 then
    Shuffle(0, fSize - 1);
end;

{--- TAbstractVector.Shuffle ---}
procedure TAbstractVector.Shuffle(PosFrom, PosTo: Integer);
var
  I, J: Integer;
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  I := PosTo;
  while I > PosFrom  do
  begin
    J := Random(I - PosFrom) + PosFrom;
    if J <> I then
      SwapFast(J, I);
    Dec(I);
  end;
end;

{--- TAbstractVector.Swap ---}
procedure TAbstractVector.Swap(I, J: Integer);
begin
  CheckIndex(I);
  CheckIndex(J);
  SwapFast(I, J);
end;

{--- TAbstractVector.ToString ---}
function TAbstractVector.ToString: String;
var
  I : Integer;
begin
  Result := '[';

  if fSize > 0 then
  begin
    for I := 0 to fSize - 2 do
      Result := Result + ItemToString(I) + ', ';
    Result := Result + ItemToString(fSize - 1);
  end;

  Result := Result + ']';
end;

{==================}
{=== TGenVector ===}
{==================}

{--- TGenVector.Append ---}
procedure TGenVector.Append(const Item: _TItem_);
begin
  Insert(fSize, Item);
end;

{--- TGenVector.AppendAll ---}
procedure TGenVector.AppendAll(Src: TGenVector);
begin
  InsertAll(fSize, Src);
end;

{--- TGenVector.AppendRange ---}
procedure TGenVector.AppendRange(Src: TGenVector; PosFrom, PosTo: Integer);
begin
  InsertRange(fSize, Src, PosFrom, PosTo);
end;

{--- TGenVector.BinarySearch ---}
function TGenVector.BinarySearch(const Item: _TItem_): Integer;
begin
  Result := BinarySearch(Item, fOnCompareItems);
end;

{--- TGenVector.BinarySearch ---}
function TGenVector.BinarySearch(const Item: _TItem_; Comparator: TCompareItems): Integer;
begin
  if fSize > 0 then
    Result := BinarySearch(Item, 0, fSize - 1, Comparator)
  else
    Result := -1;
end;

{--- TGenVector.BinarySearch ---}
function TGenVector.BinarySearch(const Item: _TItem_;
  PosFrom, PosTo: Integer): Integer;
begin
  Result := BinarySearch(Item, PosFrom, PosTo, fOnCompareItems);
end;

{--- TGenVector.BinarySearch ---}
function TGenVector.BinarySearch(const Item: _TItem_;
  PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;
var
  Low, Mid, High, Cmp : Integer;
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  Low := PosFrom;
  Mid := -1;
  High := PosTo;

  while Low <= High do
  begin
    Mid := (Low + High) div 2;
    Cmp := Comparator(fItems[Mid], Item);

    if Cmp = 0 then
    begin
      Result := Mid;
      Exit;
    end;

    if Cmp < 0 then
      Low := Mid + 1
    else
      High := Mid - 1;
  end;

  if Mid < 0 then
    Result := -1
  else if Comparator(fItems[Mid], Item) > 0 then
    Result := - Mid - 1
  else
    Result := - Mid - 2;
end;

{--- TGenVector.DefaultCompareItems ---}
function TGenVector.DefaultCompareItems(const A, B: _TItem_): Integer;
begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := 0;
end;

{--- TGenVector.Contains ---}
function TGenVector.Contains(const Item: _TItem_): Boolean;
begin
  Result := Contains(Item, fOnCompareItems);
end;

{--- TGenVector.Contains ---}
function TGenVector.Contains(const Item: _TItem_; Comparator: TCompareItems): Boolean;
begin
  if fSize = 0 then
    Result := false
  else
    Result := (FindIndex(Item, 0, Comparator) >= 0);
end;

{--- TGenVector.Create ---}
constructor TGenVector.Create(InitialCapacity: Integer);
begin
  if InitialCapacity < 0 then
    InitialCapacity := 16;

  fSize := 0;

  SetCapacity(InitialCapacity);

  SetOnCompareItems(nil);
  SetOnItemToString(nil);
end;

{--- TGenVector.Destroy ---}
destructor TGenVector.Destroy;
begin
  SetCapacity(0);
  inherited Destroy;
end;

{--- TGenVector.Equals ---}
function TGenVector.Equals(Obj: TObject): Boolean;
begin
  Result := Equals(Obj, fOnCompareItems);
end;

{--- TGenVector.Equals ---}
function TGenVector.Equals(Obj: TObject; Comparator: TCompareItems): Boolean;
var
  Vector: TGenVector;
  I : Integer;
begin
  if Obj = Self  then
    Result := true
  else if Obj is TGenVector then
  begin
    Vector := Obj as TGenVector;

    if fSize <> Vector.fSize then
      Result := false
    else
    begin
      Result := true;
      for I := 0 to fSize - 1 do
        if Comparator(fItems[I], Vector.fItems[I]) <> 0 then
        begin
          Result := false;
          Break;
        end;
    end;
  end
  else
    Result := false;
end;

{--- TGenVector.EnumeratorGet ---}
function TGenVector.EnumeratorGet(const Pos: Integer): _TItem_;
begin
  Result := fItems[Pos];
end;

{--- TGenVector.EnumeratorNext ---}
function TGenVector.EnumeratorNext(var Pos: Integer): Boolean;
begin
  Inc(Pos);
  Result := Pos < fSize;
end;

{--- TGenVector.Fill ---}
procedure TGenVector.Fill(Index, Count: Integer; const Value: _TItem_);
var
  I: Integer;
begin
  if Count > 0 then
    for I := Index to Index + (Count - 1) do
      fItems[I] := Value;
end;

{--- TGenVector.FindIndex ---}
function TGenVector.FindIndex(const Item: _TItem_): Integer;
begin
  Result := FindIndex(Item, fOnCompareItems);
end;

{--- TGenVector.FindIndex ---}
function TGenVector.FindIndex(const Item: _TItem_; Comparator: TCompareItems): Integer;
begin
  if fSize = 0 then
    Result := -1
  else
    Result := FindIndex(Item, 0, Comparator);
end;

{--- TGenVector.FindIndex ---}
function TGenVector.FindIndex(const Item: _TItem_; PosFrom: Integer): Integer;
begin
  Result := FindIndex(Item, PosFrom, fOnCompareItems);
end;

{--- TGenVector.FindIndex ---}
function TGenVector.FindIndex(const Item: _TItem_; PosFrom: Integer; Comparator: TCompareItems): Integer;
var
  I: Integer;
begin
  CheckIndex(PosFrom);

  Result := -1;

  for I := PosFrom to fSize - 1 do
    if Comparator(fItems[I], Item) = 0 then
    begin
      Result := I;
      Break;
    end;
end;

{--- TGenVector.FirstItem ---}
function TGenVector.FirstItem: _TItem_;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := fItems[0];
end;

{--- TGenVector.GetEnumerator ---}
function TGenVector.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(-1, @EnumeratorNext, @EnumeratorGet);
end;

{--- TGenVector.GetItem ---}
function TGenVector.GetItem(Position: Integer): _TItem_;
begin
  CheckIndex(Position);
  Result := fItems[Position];
end;

{--- TGenVector.GetItemFast ---}
function TGenVector.GetItemFast(Position: Integer): _TItem_;
begin
  Result := fItems[Position];
end;

{--- TGenVector.GetItemPtr ---}
function TGenVector.GetItemPtr(Position: Integer): PItem;
begin
  CheckIndex(Position);
  Result := @fItems[Position];
end;

{--- TGenVector.GetItemPtrFast ---}
function TGenVector.GetItemPtrFast(Position: Integer): PItem;
begin
  Result := @fItems[Position];
end;

{--- TGenVector.Insert ---}
procedure TGenVector.Insert(Before: Integer; const Item: _TItem_; Count: Integer);
begin
  CheckIndexForAdd(Before);

  if Count > 0 then
  begin
    InsertSpaceFast(Before, Count);
    Fill(Before, Count, Item);
  end;
end;

{--- TGenVector.InsertAll ---}
procedure TGenVector.InsertAll(Before: Integer; Src: TGenVector);
begin
  if Src.fSize > 0 then
    InsertRange(Before, Src, 0, Src.fSize - 1);
end;

{--- TGenVector.InsertionSort ---}
procedure TGenVector.InsertionSort(PosFrom, PosTo: Integer; Comparator: TCompareItems);
var
  I, J : Integer;
  Tmp, Item : _TItem_;
begin
  if PosFrom >= PosTo then
     Exit;

  for I := PosFrom + 1 to PosTo do
  begin
    Tmp := fItems[I];

    J := I - 1;
    while (J >= PosFrom) do
    begin
      Item := fItems[J];
      if Comparator(Item, Tmp) <= 0 then
        Break;
      fItems[J + 1] :=  fItems[J];
      Dec(J);
    end;

    fItems[J + 1] := Tmp;
  end;
end;

{--- TGenVector.Quicksort ---}
procedure TGenVector.Quicksort(Left, Right: Integer; Comparator: TCompareItems);
var
  I, J : Integer;
  Pivot : _TItem_;
Begin
  if Right - Left <= 15 then
  begin
    InsertionSort(Left, Right, Comparator);
    Exit;
  end;

  I := Left;
  J := Right;
  Pivot := fItems[(Left + Right) div 2];
  repeat
    while Comparator(Pivot, fItems[I]) > 0 do
      Inc(I);

    while Comparator(Pivot, fItems[J]) < 0 do
      Dec(J);

    if I <= J then
    begin
      SwapFast(I, J);
      Dec(J);
      Inc(I);
    end;
  until I > J;

  if Left < J then
    QuickSort(Left, J, Comparator);

  if I < Right then
    QuickSort(I, Right, Comparator);
end;

{--- TGenVector.InsertRange ---}
procedure TGenVector.InsertRange(Before: Integer; Src: TGenVector;
  PosFrom, PosTo: Integer);
var
  Count : Integer;
begin
  CheckIndexForAdd(Before);
  Src.CheckIndex(PosFrom);
  Src.CheckIndex(PosTo);

  Count := PosTo - PosFrom + 1;
  if Count > 0 then
  begin
    InsertSpaceFast(Before, Count);
    RealMove(Src, Self, PosFrom, Before, Count);
  end;
end;

{--- TGenVector.InsertSpaceFast ---}
procedure TGenVector.InsertSpaceFast(Position, Count: Integer);
var
  ItemsAfterPos : Integer;
begin
  if Count > 0 then
  begin
    ItemsAfterPos := fSize - Position;
    Resize(fSize + Count);
    if ItemsAfterPos > 0 then
      Move(Position, Position + Count, ItemsAfterPos);
  end;
end;

{--- TGenVector.ItemToString ---}
function TGenVector.ItemToString(Index: Integer): String;
begin
  Result := fOnItemToString(fItems[Index]);
end;

{--- TGenVector.IsSorted ---}
function TGenVector.IsSorted : Boolean;
begin
  Result := IsSorted(fOnCompareItems);
end;

{--- TGenVector.IsSorted ---}
function TGenVector.IsSorted(Comparator: TCompareItems): Boolean;
var
  I : Integer;
begin
  Result := true;

  if fSize > 1 then
    for I := 1 to fSize - 1 do
      if Comparator(fItems[I], fItems[I - 1]) < 0 then
      begin
        Result := false;
        Break;
      end;
end;

{--- TGenVector.DefaultItemToString ---}
function TGenVector.DefaultItemToString(const Item: _TItem_): String;
begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
end;

{--- TGenVector.Iterate ---}
procedure TGenVector.Iterate(Process: TProcessItem);
begin
  Iterate(Process, 0, fSize - 1);
end;

{--- TGenVector.Iterate ---}
procedure TGenVector.Iterate(Process: TProcessItem; const PosFrom, PosTo: Integer);
var
  I : Integer;
  P : PItem;
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  P := @fItems[PosFrom];
  for I := PosFrom to PosTo do
  begin
    Process(P^);
    P := P + 1;
  end;
end;

{--- TGenVector.LastItem ---}
function TGenVector.LastItem: _TItem_;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := fItems[fSize - 1];
end;

{--- TGenVector.MaxPos ---}
function TGenVector.MaxPos(PosFrom, PosTo: Integer): Integer;
begin
  Result := MaxPos(PosFrom, PosTo, fOnCompareItems);
end;

{--- TGenVector.MaxPos ---}
function TGenVector.MaxPos(PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;
var
  I : Integer;
  Max : _TItem_;
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  if PosTo < PosFrom then
  begin
    I := PosFrom;
    PosFrom := PosTo;
    PosTo := I;
  end;

  Max := fItems[PosFrom];
  Result := PosFrom;
  for I := PosFrom + 1 to PosTo do
    if Comparator(fItems[I], Max) > 0 then
    begin
      Result := I;
      Max := fItems[I];
    end;
end;

{--- TGenVector.MaxPos ---}
function TGenVector.MaxPos : Integer;
begin
  Result := MaxPos(fOnCompareItems);
end;

{--- TGenVector.MaxPos ---}
function TGenVector.MaxPos(Comparator: TCompareItems): Integer;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := MaxPos(0, fSize - 1, Comparator);
end;

{--- TGenVector.Merge ---}
procedure TGenVector.Merge(Src: TGenVector);
begin
  Merge(Src, fOnCompareItems);
end;

{--- TGenVector.Merge ---}
procedure TGenVector.Merge(Src: TGenVector; Comparator: TCompareItems);
var
  A, B, C : Integer;
begin
  if Src.fSize = 0 then
    Exit;

  if fSize = 0 then
    AppendAll(Src)
  else if Comparator(Src.FirstItem, LastItem) >= 0 then
    AppendAll(Src)
  else if Comparator(FirstItem, Src.LastItem) >= 0 then
    PrependAll(Src)
  else
  begin
    A := fSize - 1;
    B := Src.fSize - 1;

    InsertSpace(fSize, Src.fSize);
    C := fSize - 1;

    while C > 0 do
    begin
      if Comparator(fItems[A], Src.fItems[B]) > 0 then
      begin
        fItems[C] := fItems[A];
        Dec(A);
        if A < 0 then
          Break;
      end
      else
      begin
        fItems[C] := Src.fItems[B];
        Dec(B);
        if B < 0 then
          Break;
      end;
      Dec(C);
    end;

    if (C >= 0) and (B >= 0) then
      while B >= 0 do
      begin
        fItems[B] := Src.fItems[B];
        Dec(B);
      end;

  end;
  Src.Clear;
end;

{--- TGenVector.MinPos ---}
function TGenVector.MinPos(PosFrom, PosTo: Integer): Integer;
begin
  Result := MinPos(PosFrom, PosTo, fOnCompareItems);
end;

{--- TGenVector.MinPos ---}
function TGenVector.MinPos(PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;
var
  I : Integer;
  Min : _TItem_;
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  if PosTo < PosFrom then
  begin
    I := PosFrom;
    PosFrom := PosTo;
    PosTo := I;
  end;

  Result := -1;
  Min := fItems[PosFrom];
  Result := PosFrom;
  for I := PosFrom + 1 to PosTo do
    if Comparator(fItems[I], Min) < 0 then
    begin
      Result := I;
      Min := fItems[I];
    end;
end;

{--- TGenVector.MinPos ---}
function TGenVector.MinPos : Integer;
begin
  Result := MinPos(fOnCompareItems);
end;

{--- TGenVector.MinPos ---}
function TGenVector.MinPos(Comparator: TCompareItems): Integer;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := MinPos(0, fSize - 1, Comparator);
end;

{--- TGenVector.Move ---}
procedure TGenVector.Move(Src, Dst, Count: Integer);
begin
  CheckIndex(Src);
  CheckIndex(Dst);

  if Count > 0 then
  begin
    if Src + Count > fSize then
      Count := fSize - Src;

    if Dst + Count > fSize then
      Count := fSize - Dst;

    if Count > 0 then
      RealMove(Self, Self, Src, Dst, Count);
  end;
end;

{--- TGenVector.Prepend ---}
procedure TGenVector.Prepend(const Item: _TItem_; Count: Integer);
begin
  Insert(0, Item, Count);
end;

{--- TGenVector.PrependAll ---}
procedure TGenVector.PrependAll(Src: TGenVector);
begin
  InsertAll(0, Src);
end;

{--- TGenVector.PrependRange ---}
procedure TGenVector.PrependRange(Src: TGenVector; PosFrom, PosTo: Integer);
begin
  InsertRange(0, Src, PosFrom, PosTo);
end;

{--- TGenVector.ReadFirstItem ---}
procedure TGenVector.ReadFirstItem(out Value : _TItem_);
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Value := fItems[0];
end;

{--- TGenVector.ReadItem ---}
procedure TGenVector.ReadItem(Position: Integer; out Value: _TItem_);
begin
  CheckIndex(Position);
  Value := fItems[Position];
end;

{--- TGenVector.ReadItemFast ---}
procedure TGenVector.ReadItemFast(Position: Integer; out Value: _TItem_);
begin
  Value := fItems[Position];
end;

{--- TGenVector.ReadLastItem ---}
procedure TGenVector.ReadLastItem(out Value : _TItem_);
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Value := fItems[fSize - 1];
end;

{--- TGenVector.Sort ---}
procedure TGenVector.Sort(PosFrom, PosTo: Integer);
begin
  Sort(PosFrom, PosTo, fOnCompareItems);
end;

{--- TGenVector.Sort ---}
procedure TGenVector.Sort(PosFrom, PosTo: Integer; Comparator: TCompareItems);
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  if PosFrom >= PosTo then
    Exit;

  Quicksort(PosFrom, PosTo, Comparator);
end;

{--- TGenVector.Sort ---}
procedure TGenVector.Sort;
begin
  Sort(fOnCompareItems);
end;

{--- TGenVector.Sort ---}
procedure TGenVector.Sort(Comparator: TCompareItems);
begin
  if fSize > 1 then
    Sort(0, fSize - 1, Comparator);
end;

{--- TGenVector.RealMove ---}
class procedure TGenVector.RealMove(Src, Dst: TGenVector;
  SrcFirst, DstFirst, Count: Integer);
var
  SrcLast, I, DstCurrent: Integer;
begin
  SrcLast := SrcFirst + Count - 1;
  if (Src = Dst) and ( (DstFirst >= SrcFirst) and (DstFirst <= SrcLast) ) then
  begin
    DstCurrent := DstFirst + Count - 1;
    for I := SrcLast downto SrcFirst do
    begin
      Dst.fItems[DstCurrent] := Src.fItems[I];
      Dec(DstCurrent);
    end
  end
  else
  begin
    DstCurrent := DstFirst;
    for I := SrcFirst to SrcLast do
    begin
      Dst.fItems[DstCurrent] := Src.fItems[I];
      Inc(DstCurrent);
    end;
  end;
end;

{--- TGenVector.Replace ---}
procedure TGenVector.Replace(Index, Count: Integer; const Value: _TItem_);
begin
  CheckIndex(Index);

  if Count > 0 then
  begin
    if Index + Count >= fSize then
      Count := fSize - Index;

    if Count > 0 then
      Fill(Index, Count, Value);
  end;
end;

{--- TGenVector.ReverseFindIndex ---}
function TGenVector.ReverseFindIndex(const Item: _TItem_): Integer;
begin
  Result := ReverseFindIndex(Item, fOnCompareItems);
end;

{--- TGenVector.ReverseFindIndex ---}
function TGenVector.ReverseFindIndex(const Item: _TItem_; Comparator: TCompareItems): Integer;
begin
  if fSize = 0 then
    Result := -1
  else
    Result := ReverseFindIndex(Item, fSize - 1, Comparator);
end;

{--- TGenVector.ReverseFindIndex ---}
function TGenVector.ReverseFindIndex(const Item: _TItem_;
  PosFrom: Integer): Integer;
begin
  Result := ReverseFindIndex(Item, PosFrom, fOnCompareItems);
end;

{--- TGenVector.ReverseFindIndex ---}
function TGenVector.ReverseFindIndex(const Item: _TItem_;
  PosFrom: Integer; Comparator: TCompareItems): Integer;
var
  I: Integer;
begin
  CheckIndex(PosFrom);

  Result := -1;
  for I := PosFrom downto 0 do
    if Comparator(fItems[I], Item) = 0 then
    begin
      Result := I;
      Break;
    end;
end;

{--- TGenVector.SetCapacity ---}
procedure TGenVector.SetCapacity(ACapacity: Integer);
begin
  SetLength(fItems, ACapacity);
  fCapacity := ACapacity;
end;

{--- TGenVector.SetOnCompareItems ---}
procedure TGenVector.SetOnCompareItems(AValue: TCompareItems);
begin
  if AValue = nil then
    fOnCompareItems := @DefaultCompareItems
  else
    fOnCompareItems := AValue;
end;

{--- TGenVector.SetOnItemToString ---}
procedure TGenVector.SetOnItemToString(AValue: TItemToString);
begin
  if AValue = nil then
    fOnItemToString := @DefaultItemToString
  else
    fOnItemToString := AValue;
end;

{--- TGenVector.SetItem ---}
procedure TGenVector.SetItem(Position: Integer; const Value: _TItem_);
begin
  CheckIndex(Position);
  fItems[Position] := Value;
end;

{--- TGenVector.SetItemFast ---}
procedure TGenVector.SetItemFast(Position: Integer; const Value: _TItem_);
begin
  fItems[Position] := Value;
end;

{--- TGenVector.SwapFast ---}
procedure TGenVector.SwapFast(I, J: Integer);
var
  Temp: _TItem_;
begin
  Temp := fItems[I];
  fItems[I] := fItems[J];
  fItems[J] := Temp;
end;

{=================}
{=== TGenDeque ===}
{=================}

{--- TGenDeque.Append ---}
procedure TGenDeque.Append(const Item: _TItem_; Count: Integer);
begin
  Insert(fSize, Item, Count);
end;

{--- TGenDeque.AppendAll ---}
procedure TGenDeque.AppendAll(Src: TGenDeque);
begin
  InsertAll(fSize, Src);
end;

{--- TGenDeque.AppendRange ---}
procedure TGenDeque.AppendRange(Src: TGenDeque; PosFrom, PosTo: Integer);
begin
  InsertRange(fSize, Src, PosFrom, PosTo);
end;

{--- TGenDeque.BinarySearch ---}
function TGenDeque.BinarySearch(const Item: _TItem_): Integer;
begin
  Result := BinarySearch(Item, fOnCompareItems);
end;

{--- TGenDeque.BinarySearch ---}
function TGenDeque.BinarySearch(const Item: _TItem_; Comparator: TCompareItems): Integer;
begin
  if fSize > 0 then
    Result := BinarySearch(Item, 0, fSize - 1, Comparator)
  else
    Result := -1;
end;

{--- TGenDeque.BinarySearch ---}
function TGenDeque.BinarySearch(const Item: _TItem_; PosFrom, PosTo: Integer): Integer;
begin
  Result := BinarySearch(Item, PosFrom, PosTo, fOnCompareItems);
end;

{--- TGenDeque.BinarySearch ---}
function TGenDeque.BinarySearch(const Item: _TItem_;
  PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;
var
  Low, Mid, High, Cmp: Integer;
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  Low := PosFrom;
  Mid := -1;
  High := PosTo;

  while Low <= High do
  begin
    Mid := (Low + High) div 2;
    Cmp := Comparator(fItems[ IndexToRank(Mid) ], Item);

    if Cmp = 0 then
    begin
      Result := Mid;
      Exit;
    end;

    if Cmp < 0 then
      Low := Mid + 1
    else
      High := Mid - 1;
  end;

  if Mid < 0 then
    Result := -1
  else if Comparator(fItems[ IndexToRank(Mid) ], Item) > 0 then
    Result := - Mid - 1
  else
    Result := - Mid - 2;
end;

{--- TGenDeque.DefaultCompareItems ---}
function TGenDeque.DefaultCompareItems(const A, B: _TItem_): Integer;
begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := 0;
end;

{--- TGenDeque.Contains ---}
function TGenDeque.Contains(const Item: _TItem_): Boolean;
begin
  Result := Contains(Item, fOnCompareItems);
end;

{--- TGenDeque.Contains ---}
function TGenDeque.Contains(const Item: _TItem_; Comparator: TCompareItems): Boolean;
begin
  Result := (FindIndex(Item, Comparator) >= 0);
end;

{--- TGenDeque.Create ---}
constructor TGenDeque.Create(InitialCapacity: Integer);
begin
  fSize := 0;

  if InitialCapacity < 0 then
    InitialCapacity := 16;

  fCapacity := InitialCapacity;
  SetLength(fItems, fCapacity);

  fStart := 0;

  SetOnCompareItems(nil);
  SetOnItemToString(nil);
end;

{--- TGenDeque.Destroy ---}
destructor TGenDeque.Destroy;
begin
  SetLength(fItems, 0);
  inherited Destroy;
end;

{--- TGenDeque.DecRank ---}
procedure TGenDeque.DecRank(var Rank: Integer);
begin
  if Rank = 0 then
    Rank := fCapacity - 1
  else
    Dec(Rank);
end;

{--- TGenDeque.Equals ---}
function TGenDeque.Equals(Deque: TGenDeque; Comparator: TCompareItems): Boolean;
var
  I, IRank, JRank : Integer;
begin
  if fSize <> Deque.fSize then
    Result := false
  else
  begin
    Result := true;
    IRank := fStart;
    JRank := Deque.fStart;
    for I := 0 to fSize - 1 do
    begin
      if Comparator(fItems[IRank], Deque.fItems[JRank]) <> 0 then
      begin
        Result := false;
        Break;
      end;
      IncRank(IRank);
      Deque.IncRank(JRank);
    end;
  end;
end;

{--- TGenDeque.EnumeratorGet ---}
function TGenDeque.EnumeratorGet(const Pos: Integer): _TItem_;
begin
  Result := fItems[ IndexToRank(Pos) ];
end;

{--- TGenDeque.EnumeratorNext ---}
function TGenDeque.EnumeratorNext(var Pos: Integer): Boolean;
begin
  Inc(Pos);
  Result := Pos < fSize;
end;

{--- TGenDeque.Equals ---}
function TGenDeque.Equals(Obj: TObject): Boolean;
begin
  Result := Equals(Obj, fOnCompareItems);
end;

{--- TGenDeque.Equals ---}
function TGenDeque.Equals(Obj: TObject; Comparator: TCompareItems): Boolean;
begin
  if Obj = Self  then
    Result := true
  else if Obj is TGenDeque then
    Result := Equals(Obj as TGenDeque, Comparator)
  else
    Result := false;
end;

{--- TGenDeque.FindIndex ---}
function TGenDeque.FindIndex(const Item: _TItem_): Integer;
begin
  Result := FindIndex(Item, fOnCompareItems);
end;

{--- TGenDeque.FindIndex ---}
function TGenDeque.FindIndex(const Item: _TItem_; Comparator: TCompareItems): Integer;
begin
  if fSize = 0 then
    Result := -1
  else
    Result := FindIndex(Item, 0, Comparator);
end;

{--- TGenDeque.Fill ---}
procedure TGenDeque.Fill(Index, Count: Integer; const Value: _TItem_);
begin
  Index := IndexToRank(Index);
  while Count > 0 do
  begin
    fItems[Index] := Value;
    IncRank(Index);
    Dec(Count);
  end;
end;

{--- TGenDeque.FindIndex ---}
function TGenDeque.FindIndex(const Item: _TItem_; PosFrom: Integer): Integer;
begin
  Result := FindIndex(Item, PosFrom, fOnCompareItems);
end;

{--- TGenDeque.FindIndex ---}
function TGenDeque.FindIndex(const Item: _TItem_; PosFrom: Integer; Comparator: TCompareItems): Integer;
var
  I, Pos : Integer;
begin
  CheckIndex(PosFrom);

  Result := -1;
  Pos := IndexToRank(PosFrom);
  for I := PosFrom to fSize - 1 do
  begin
    if Comparator(fItems[Pos], Item) = 0 then
    begin
      Result := I;
      Break;
    end;
    IncRank(Pos);
  end;
end;

{--- TGenDeque.FirstItem ---}
function TGenDeque.FirstItem: _TItem_;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := fItems[fStart];
end;

{--- TGenDeque.GetEnumerator ---}
function TGenDeque.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(-1, @EnumeratorNext, @EnumeratorGet);
end;

{--- TGenDeque.GetItem ---}
function TGenDeque.GetItem(Position: Integer): _TItem_;
begin
  CheckIndex(Position);
  Result := fItems[ IndexToRank(Position)];
end;

{--- TGenDeque.GetItemPtr ---}
function TGenDeque.GetItemPtr(Position: Integer): PItem;
begin
  CheckIndex(Position);
  Result := @fItems[ IndexToRank(Position)];
end;

{--- TGenDeque.GetItemFast ---}
function TGenDeque.GetItemFast(Position: Integer): _TItem_;
begin
  Result := fItems[ IndexToRank(Position) ];
end;

{--- TGenDeque.GetItemPtrFast ---}
function TGenDeque.GetItemPtrFast(Position: Integer): PItem;
begin
  Result := @fItems[ IndexToRank(Position) ];
end;

{--- TGenDeque.IncRank ---}
procedure TGenDeque.IncRank(var Rank: Integer);
begin
  if Rank = fCapacity - 1 then
    Rank := 0
  else
    Inc(Rank);
end;

{--- TGenDeque.IncreaseCapacity ---}
procedure TGenDeque.IncreaseCapacity(ACapacity: Integer);
var
  Dst : Integer;
  ItemsAtBegining, ItemsAtEnd : Integer;
begin
  SetLength(fItems, ACapacity);

  if fStart + fSize >= fCapacity then { Are items in 2 parts ? }
  begin
    ItemsAtEnd := fCapacity - fStart;
    ItemsAtBegining := fSize - ItemsAtEnd;

    if ItemsAtEnd < ItemsAtBegining then
    begin
      Dst := ACapacity - ItemsAtEnd;
      RealMoveRank(fStart, Dst, ItemsAtEnd);
      fStart := Dst;
    end
    else
    begin
      Dst := fStart + ItemsAtEnd;
      RealMoveRank(0, Dst, ItemsAtBegining);
    end;
  end;

  fCapacity := ACapacity;
end;

{--- TGenDeque.IndexToRank ---}
function TGenDeque.IndexToRank(Index: Integer): Integer;
var
  AtEnd : Integer;
begin
  AtEnd := fCapacity - fStart;
  if Index < AtEnd then
    Result := fStart + Index
  else
    Result := Index - AtEnd;
end;

{--- TGenDeque.Insert ---}
procedure TGenDeque.Insert(Before: Integer; const Item: _TItem_; Count: Integer);
begin
  CheckIndexForAdd(Before);

  if Count <= 0 then
    Exit;

  InsertSpaceFast(Before, Count);
  Fill(Before, Count, Item);
end;

{--- TGenDeque.InsertAll ---}
procedure TGenDeque.InsertAll(Before: Integer; Src: TGenDeque);
begin
  if Src.fSize > 0 then
    InsertRange(Before, Src, 0, Src.fSize - 1);
end;

{--- TGenDeque.InsertionSort ---}
procedure TGenDeque.InsertionSort(PosFrom, PosTo: Integer; Comparator: TCompareItems);
var
  I, J : Integer;
  IRank, JRank, NextJRank: Integer;
  Tmp, Item : _TItem_;
begin
  if PosFrom >= PosTo then
     Exit;

  IRank := IndexToRank(PosFrom + 1);
  for I := PosFrom + 1 to PosTo do
  begin
    Tmp := fItems[IRank];

    J := I - 1;
    JRank := IRank;
    DecRank(JRank);
    while (J >= PosFrom) do
    begin
      Item := fItems[JRank];
      if Comparator(Item, Tmp) <= 0 then
        Break;
      NextJRank := JRank;
      IncRank(NextJRank);
      fItems[NextJRank] :=  fItems[JRank];
      Dec(J);
      DecRank(JRank);
    end;

    fItems[IndexToRank(J + 1)] := Tmp;
    IncRank(IRank);
  end;
end;

{--- TGenDeque.Quicksort ---}
procedure TGenDeque.Quicksort(Left, Right: Integer; Comparator: TCompareItems);
var
  I, J : Integer;
  Pivot : _TItem_;
Begin
  if Right - Left <= 15 then
  begin
    InsertionSort(Left, Right, Comparator);
    Exit;
  end;

  I := Left;
  J := Right;
  Pivot := fItems[ IndexToRank((Left + Right) div 2) ];
  repeat
    while Comparator(Pivot, fItems[IndexToRank(I)]) > 0 do
      Inc(I);

    while Comparator(Pivot, fItems[IndexToRank(J)]) < 0 do
      Dec(J);

    if I <= J then
    begin
      SwapFast(I, J);
      Dec(J);
      Inc(I);
    end;
  until I > J;

  if Left < J then
    QuickSort(Left, J, Comparator);

  if I < Right then
    QuickSort(I, Right, Comparator);
end;

{--- TGenDeque.InsertRange ---}
procedure TGenDeque.InsertRange(Before: Integer; Src: TGenDeque;
  PosFrom, PosTo: Integer);
var
  Count : Integer;
begin
  CheckIndexForAdd(Before);
  Src.CheckIndex(PosFrom);
  Src.CheckIndex(PosTo);

  Count := PosTo - PosFrom + 1;
  if Count > 0 then
  begin
    InsertSpaceFast(Before, Count);
    RealMoveIndex(Src, Self, PosFrom, Before, Count);
  end;
end;

{--- TGenDeque.InsertSpaceFast ---}
procedure TGenDeque.InsertSpaceFast(Position, Count: Integer);
var
  Rank : Integer;
  NewStart : Integer;
  ItemsToMove : Integer;
begin
  if Count <= 0 then
    Exit;

  if Position = 0 then
  begin
    Resize(fSize + Count);

    NewStart := fStart - Count;
    if NewStart < 0 then
      fStart := fCapacity + NewStart
    else
      fStart := NewStart;
  end
  else if Position = fSize then
  begin
    Resize(fSize + Count);
  end
  else
  begin
    Resize(fSize + Count);
    Rank := IndexToRank(Position);

    if (Rank >= fStart) and (fStart + fSize > fCapacity) then
    begin
      ItemsToMove := Rank - fStart;
      if ItemsToMove > 0 then
        RealMoveRank(fStart, fStart - Count , ItemsToMove);
      fStart := fStart - Count;
    end
    else
    begin
      ItemsToMove :=  fSize - Position - Count;

      if ItemsToMove > 0 then
        RealMoveRank(Rank, Rank + Count, ItemsToMove)
    end;
  end;
end;

{--- TGenDeque.ItemToString ---}
function TGenDeque.ItemToString(Index: Integer): String;
begin
  Result := fOnItemToString(fItems[IndexToRank(Index)]);
end;

{--- TGenDeque.RankToIndex ---}
function TGenDeque.RankToIndex(Rank: Integer): Integer;
begin
  if Rank >= fStart then
    Result := Rank - fStart
  else
    Result := Rank + (fCapacity - fStart);
end;

{--- TGenDeque.IsSorted ---}
function TGenDeque.IsSorted : Boolean;
begin
  Result := IsSorted(fOnCompareItems);
end;

{--- TGenDeque.IsSorted ---}
function TGenDeque.IsSorted(Comparator: TCompareItems): Boolean;
var
  I, Rank, PrevRank: Integer;
begin
  Result := true;

  if fSize > 1 then
  begin
    PrevRank := fStart;
    Rank := IndexToRank(1);
    for I := 1 to fSize - 1 do
    begin
      if Comparator(fItems[Rank], fItems[PrevRank]) < 0 then
      begin
        Result := false;
        Break;
      end;
      PrevRank := Rank;
      IncRank(Rank);
    end;
  end;
end;

{--- TGenDeque.DefaultItemToString ---}
function TGenDeque.DefaultItemToString(const Item: _TItem_): String;
begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
end;

{--- TGenDeque.Iterate ---}
procedure TGenDeque.Iterate(Process: TProcessItem);
begin
  Iterate(Process, 0, fSize - 1);
end;

{--- TGenDeque.Iterate ---}
procedure TGenDeque.Iterate(Process: TProcessItem; const PosFrom, PosTo: Integer);
var
  I, Rank : Integer;
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  Rank := IndexToRank(PosFrom);
  for I := PosFrom to PosTo do
  begin
    Process(fItems[Rank]);
    IncRank(Rank);
  end;
end;

{--- TGenDeque.LastItem ---}
function TGenDeque.LastItem: _TItem_;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := fItems[ IndexToRank(fSize - 1) ];
end;

{--- TGenDeque.MaxPos ---}
function TGenDeque.MaxPos(PosFrom, PosTo: Integer): Integer;
begin
  Result := MaxPos(PosFrom, PosTo, fOnCompareItems);
end;

{--- TGenDeque.MaxPos ---}
function TGenDeque.MaxPos(PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;
var
  I, IRank : Integer;
  Max : _TItem_;
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  if PosTo < PosFrom then
  begin
    I := PosFrom;
    PosFrom := PosTo;
    PosTo := I;
  end;

  Max := fItems[ IndexToRank(PosFrom) ];
  Result := PosFrom;
  IRank := IndexToRank(PosFrom + 1);
  for I := PosFrom + 1 to PosTo do
  begin
    if Comparator(fItems[IRank], Max) > 0 then
    begin
      Result := I;
      Max := fItems[IRank];
    end;
    IncRank(IRank);
  end;
end;

{--- TGenDeque.MaxPos ---}
function TGenDeque.MaxPos : Integer;
begin
  Result := MaxPos(fOnCompareItems);
end;

{--- TGenDeque.MaxPos ---}
function TGenDeque.MaxPos(Comparator: TCompareItems): Integer;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := MaxPos(0, fSize - 1, Comparator);
end;

{--- TGenDeque.Merge ---}
procedure TGenDeque.Merge(Src: TGenDeque);
begin
  Merge(Src, fOnCompareItems);
end;

{--- TGenDeque.Merge ---}
procedure TGenDeque.Merge(Src: TGenDeque; Comparator: TCompareItems);
var
  A, B, C : Integer;
  ARank, BRank, CRank : Integer;
begin
  if Src.fSize = 0 then
    Exit;

  if fSize = 0 then
    AppendAll(Src)
  else if Comparator(Src.FirstItem, LastItem) >= 0 then
    AppendAll(Src)
  else if Comparator(FirstItem, Src.LastItem) >= 0 then
    PrependAll(Src)
  else
  begin
    A := fSize - 1;
    B := Src.fSize - 1;

    InsertSpace(fSize, Src.fSize);
    C := fSize - 1;

    ARank := IndexToRank(A);
    BRank := Src.IndexToRank(B);
    CRank := IndexToRank(C);

    while C > 0 do
    begin
      if Comparator(fItems[ARank], Src.fItems[BRank]) > 0 then
      begin
        fItems[CRank] := fItems[ARank];
        Dec(A);
        if A < 0 then
          Break;
        DecRank(ARank);
      end
      else
      begin
        fItems[CRank] := Src.fItems[BRank];
        Dec(B);
        if B < 0 then
          Break;
        Src.DecRank(BRank);
      end;
      Dec(C);
      DecRank(CRank);
    end;

    if (C >= 0) and (B >= 0) then
    begin
      BRank := Src.IndexToRank(B);
      ARank := IndexToRank(B);
      while B >= 0 do
      begin
        fItems[ARank] := Src.fItems[BRank];
        Dec(B);
        DecRank(BRank);
        DecRank(ARank);
      end;
    end;

  end;
  Src.Clear;
end;

{--- TGenDeque.MinPos ---}
function TGenDeque.MinPos(PosFrom, PosTo: Integer): Integer;
begin
  Result := MinPos(PosFrom, PosTo, fOnCompareItems);
end;

{--- TGenDeque.MinPos ---}
function TGenDeque.MinPos(PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;
var
  I, IRank : Integer;
  Min : _TItem_;
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  if PosTo < PosFrom then
  begin
    I := PosFrom;
    PosFrom := PosTo;
    PosTo := I;
  end;

  Result := -1;
  Min := fItems[ IndexToRank(PosFrom) ];
  Result := PosFrom;
  IRank := IndexToRank(PosFrom + 1);
  for I := PosFrom + 1 to PosTo do
  begin
    if Comparator(fItems[IRank], Min) < 0 then
    begin
      Result := I;
      Min := fItems[IRank];
    end;
    IncRank(IRank);
  end;
end;

{--- TGenDeque.MinPos ---}
function TGenDeque.MinPos: Integer;
begin
  Result := MinPos(fOnCompareItems);
end;

{--- TGenDeque.MinPos ---}
function TGenDeque.MinPos(Comparator: TCompareItems): Integer;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := MinPos(0, fSize - 1, Comparator);
end;

{--- TGenDeque.Move ---}
procedure TGenDeque.Move(Src, Dst, Count: Integer);
var
  I: Integer;
begin
  CheckIndex(Src);
  CheckIndex(Dst);

  if Src + Count > fSize then
    Count := fSize - Src;

  if Dst + Count > fSize then
    Count := fSize - Dst;

  if Count > 0 then
  begin
    if (Dst >= Src) and (Dst <= Src + Count - 1) then
    begin
      Dst := Dst + Count - 1;
      Src := Src + Count - 1;

      Dst := IndexToRank(Dst);
      Src := IndexToRank(Src);

      for I := 1 to Count do
      begin
        fItems[Dst] := fItems[Src];
        DecRank(Src);
        DecRank(Dst);
      end;
    end
    else
    begin
      Dst := IndexToRank(Dst);
      Src := IndexToRank(Src);

      for I := 1 to Count do
      begin
        fItems[Dst] := fItems[Src];
        IncRank(Src);
        IncRank(Dst);
      end;
    end;
  end;
end;

{--- TGenDeque.Prepend ---}
procedure TGenDeque.Prepend(const Item: _TItem_; Count: Integer);
begin
  Insert(0, Item, Count);
end;

{--- TGenDeque.PrependAll ---}
procedure TGenDeque.PrependAll(Src: TGenDeque);
begin
  InsertAll(0, Src);
end;

{--- TGenDeque.PrependRange ---}
procedure TGenDeque.PrependRange(Src: TGenDeque; PosFrom, PosTo: Integer);
begin
  InsertRange(0, Src, PosFrom, PosTo);
end;

{--- TGenDeque.ReadFirstItem ---}
procedure TGenDeque.ReadFirstItem(out Value : _TItem_);
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Value := fItems[fStart];
end;

{--- TGenDeque.ReadItem ---}
procedure TGenDeque.ReadItem(Position: Integer; out Value: _TItem_);
begin
  CheckIndex(Position);
  Value := fItems[ IndexToRank(Position)];
end;

{--- TGenDeque.ReadItemFast ---}
procedure TGenDeque.ReadItemFast(Position: Integer; out Value: _TItem_);
begin
  Value := fItems[ IndexToRank(Position)];
end;

{--- TGenDeque.ReadLastItem ---}
procedure TGenDeque.ReadLastItem(out Value : _TItem_);
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Value := fItems[ IndexToRank(fSize - 1) ];
end;

{--- TGenDeque.Sort ---}
procedure TGenDeque.Sort(PosFrom, PosTo: Integer);
begin
  Sort(PosFrom, PosTo, fOnCompareItems);
end;

{--- TGenDeque.Sort ---}
procedure TGenDeque.Sort(PosFrom, PosTo: Integer; Comparator: TCompareItems);
begin
  CheckIndex(PosFrom);
  CheckIndex(PosTo);

  if PosFrom >= PosTo then
    Exit;

  Quicksort(PosFrom, PosTo, Comparator);
end;

{--- TGenDeque.Sort ---}
procedure TGenDeque.Sort;
begin
  Sort(fOnCompareItems);
end;

{--- TGenDeque.Sort ---}
procedure TGenDeque.Sort(Comparator: TCompareItems);
begin
  if fSize > 1 then
    Sort(0, fSize - 1, Comparator);
end;

{--- TGenDeque.RealMoveRank ---}
procedure TGenDeque.RealMoveRank(Src, Dst, Count: Integer);
var
  SrcLast, I, DstCurrent: Integer;
begin
  if Count <= 0 then
    Exit;

  SrcLast := Src + Count - 1;
  if (Dst >= Src) and (Dst <= SrcLast) then
  begin
    DstCurrent := Dst + Count - 1;
    for I := SrcLast downto Src do
    begin
      fItems[DstCurrent] := fItems[I];
      Dec(DstCurrent);
    end
  end
  else
  begin
    DstCurrent := Dst;
    for I := Src to SrcLast do
    begin
      fItems[DstCurrent] := fItems[I];
      Inc(DstCurrent);
    end;
  end;
end;

{--- TGenDeque.RealMoveIndex ---}
class procedure TGenDeque.RealMoveIndex(Src, Dst: TGenDeque;
  SrcFirst, DstFirst, Count: Integer);
var
  SrcLast, I, DstCurrent: Integer;
begin
  SrcLast := SrcFirst + Count - 1;
  if (Src = Dst) and ( (DstFirst >= SrcFirst) and (DstFirst <= SrcLast) ) then
  begin
    DstCurrent := DstFirst + Count - 1;
    for I := SrcLast downto SrcFirst do
    begin
      Dst[DstCurrent] := Src[I];
      Dec(DstCurrent);
    end
  end
  else
  begin
    DstCurrent := DstFirst;
    for I := SrcFirst to SrcLast do
    begin
      Dst[DstCurrent] := Src[I];
      Inc(DstCurrent);
    end;
  end;
end;

{--- TGenDeque.ReduceCapacity ---}
procedure TGenDeque.ReduceCapacity(ACapacity: Integer);
var
  NewStart, ItemsAtEnd : Integer;
begin
  if fStart + fSize >= fCapacity then
  begin
    ItemsAtEnd := fCapacity - fStart;
    NewStart := ACapacity - ItemsAtEnd;
    RealMoveRank(fStart, NewStart, ItemsAtEnd);
    fStart := NewStart;
  end;

  SetLength(fItems, ACapacity);
  fCapacity := ACapacity;
end;

{--- TGenDeque.Replace ---}
procedure TGenDeque.Replace(Index, Count: Integer; const Value: _TItem_);
begin
  CheckIndex(Index);

  if Count > 0 then
  begin
    if Index + Count >= fSize then
      Count := fSize - Index;
    Fill(Index, Count, Value);
  end;
end;

{--- TGenDeque.ReverseFindIndex ---}
function TGenDeque.ReverseFindIndex(const Item: _TItem_): Integer;
begin
  Result := ReverseFindIndex(Item, fOnCompareItems);
end;

{--- TGenDeque.ReverseFindIndex ---}
function TGenDeque.ReverseFindIndex(const Item: _TItem_; Comparator: TCompareItems): Integer;
begin
  if fSize = 0 then
    Result := -1
  else
    Result := ReverseFindIndex(Item, fSize - 1, Comparator);
end;

{--- TGenDeque.ReverseFindIndex ---}
function TGenDeque.ReverseFindIndex(const Item: _TItem_; PosFrom: Integer): Integer;
begin
  Result := ReverseFindIndex(Item, PosFrom, fOnCompareItems);
end;

{--- TGenDeque.ReverseFindIndex ---}
function TGenDeque.ReverseFindIndex(const Item: _TItem_;
  PosFrom: Integer; Comparator: TCompareItems): Integer;
var
  I, Pos: Integer;
begin
  CheckIndex(PosFrom);

  Result := -1;
  Pos := IndexToRank(PosFrom);
  for I := PosFrom downto 0 do
  begin
    if Comparator(fItems[Pos], Item) = 0 then
    begin
      Result := I;
      Break;
    end;
    DecRank(Pos);
  end;
end;

{--- TGenDeque.SetCapacity ---}
procedure TGenDeque.SetCapacity(ACapacity: Integer);
begin
  if ACapacity <= fCapacity then
    ReduceCapacity(ACapacity)
  else if ACapacity > fCapacity then
    IncreaseCapacity(ACapacity);
end;

{--- TGenDeque.SetOnCompareItems ---}
procedure TGenDeque.SetOnCompareItems(AValue: TCompareItems);
begin
  if AValue = nil then
    fOnCompareItems := @DefaultCompareItems
  else
    fOnCompareItems := AValue;
end;

{--- TGenDeque.SetOnItemToString ---}
procedure TGenDeque.SetOnItemToString(AValue: TItemToString);
begin
  if AValue = nil then
    fOnItemToString := @DefaultItemToString
  else
    fOnItemToString := AValue;
end;

{--- TGenDeque.SetItem ---}
procedure TGenDeque.SetItem(Position: Integer; const Value: _TItem_);
begin
  CheckIndex(Position);
  fItems[ IndexToRank(Position) ] := Value;
end;

{--- TGenDeque.SetItemFast ---}
procedure TGenDeque.SetItemFast(Position: Integer; const Value: _TItem_);
begin
  fItems[ IndexToRank(Position) ] := Value;
end;

{--- TGenDeque.SwapFast ---}
procedure TGenDeque.SwapFast(I, J: Integer);
var
  Temp: _TItem_;
begin
  I := IndexToRank(I);
  J := IndexToRank(J);

  Temp := fItems[I];
  fItems[I] := fItems[J];
  fItems[J] := Temp;
end;

{===================}
{=== TListCursor ===}
{===================}

{--- TListCursor.Equals ---}
function TListCursor.Equals(const Cursor: TListCursor): Boolean;
begin
  Result := (fList = Cursor.fList) and (fNode = Cursor.fNode);
end;

{--- TListCursor.HasItem ---}
function TListCursor.HasItem: Boolean;
begin
  Result := (fNode <> nil);
end;

{--- TListCursor.Init ---}
constructor TListCursor.Init(AList: TAbstractList; ANode: Pointer);
begin
  fList := AList;
  fNode := ANode;
end;

{--- TListCursor.IsFirst ---}
function TListCursor.IsFirst: Boolean;
begin
  Result := fList.CursorIsFirst(Self);
end;

{--- TListCursor.IsLast ---}
function TListCursor.IsLast: Boolean;
begin
  Result := fList.CursorIsLast(Self);
end;

{--- TListCursor.IsNil ---}
function TListCursor.IsNil: Boolean;
begin
  Result := (fNode = nil);
end;

{--- TListCursor.MoveNext ---}
procedure TListCursor.MoveNext;
begin
  fList.CursorMoveNext(Self);
end;

{--- TListCursor.MovePrevious ---}
procedure TListCursor.MovePrevious;
begin
  fList.CursorMovePrev(Self);
end;

{=====================}
{=== TAbstractList ===}
{=====================}

{--- TAbstractList.CheckValid ---}
procedure TAbstractList.CheckValid(const Cursor: TListCursor);
begin
  if Cursor.List <> Self then
    RaiseCursorDenotesWrongContainer;
end;

{--- TAbstractList.CheckNotNil ---}
procedure TAbstractList.CheckNotNil(const Cursor: TListCursor);
begin
  CheckValid(Cursor);
  if Cursor.IsNil then
    RaiseCursorIsNil;
end;

{================}
{=== TGenList ===}
{================}

{--- TGenList.Append ---}
procedure TGenList.Append(const Item: _TItem_; Count: Integer);
begin
  Insert(fNilCursor, Item, Count);
end;

{--- TGenList.AppendAll ---}
procedure TGenList.AppendAll(Src: TGenList);
begin
  InsertAll(fNilCursor, Src);
end;

{--- TGenList.AppendRange ---}
procedure TGenList.AppendRange(Src: TGenList; const PosFrom, PosTo: TListCursor);
begin
  InsertRange(fNilCursor, Src, PosFrom, PosTo);
end;

{--- TGenList.Clear ---}
procedure TGenList.Clear;
begin
  DeleteFirst(fSize);
end;

{--- TGenList.DefaultCompareItems ---}
function TGenList.DefaultCompareItems(const A, B: _TItem_): Integer;
begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := 0;
end;

{--- TGenList.Contains ---}
function TGenList.Contains(const Item: _TItem_): Boolean;
begin
  Result := Contains(Item, fOnCompareItems);
end;

{--- TGenList.Contains ---}
function TGenList.Contains(const Item: _TItem_; Comparator: TCompareItems): Boolean;
begin
  Result := not Find(Item, Comparator).IsNil;
end;

{--- TGenList.Create ---}
constructor TGenList.Create;
begin
  inherited Create;

  New(fHead);
  New(fTail);
  fHead^.Next := fTail;
  fTail^.Previous := fHead;

  fNilCursor.Init(Self, nil);

  SetOnCompareItems(nil);
  SetOnItemToString(nil);
end;

{--- TGenList.Delete ---}
procedure TGenList.Delete(var Position: TListCursor; Count: Integer);
begin
  CheckNotNil(Position);
  DeleteNodesForward(PNode(Position.Node), Count);
  Position := fNilCursor;
end;

{--- TGenList.DeleteFirst ---}
procedure TGenList.DeleteFirst(Count: Integer);
begin
  if (fSize > 0) and (Count > 0) then
    DeleteNodesForward(fHead^.Next, Count);
end;

{--- TGenList.DeleteLast ---}
procedure TGenList.DeleteLast(Count: Integer);
begin
  if (fSize > 0) and (Count > 0) then
    DeleteNodesBackward(fTail^.Previous, Count);
end;

{--- TGenList.DeleteNodesBackward ---}
procedure TGenList.DeleteNodesBackward(From: PNode; Count: Integer);
var
  Current, AfterFrom : PNode;
begin
  AfterFrom := From^.Next;

  Current := From;
  while (Count > 0) and (Current <> fHead) do
  begin
    Current^.Previous^.Next := AfterFrom;
    AfterFrom^.Previous := Current^.Previous;

    Dispose(Current);
    Dec(fSize);
    Dec(Count);
    Current := AfterFrom^.Previous;
  end;
end;

{--- TGenList.DeleteNodesBetween ---}
procedure TGenList.DeleteNodesBetween(NodeFrom, NodeTo: PNode);
var
  Current, Previous, Limit: PNode;
begin
  Current := NodeFrom;
  Previous := Current^.Previous;
  Limit := NodeTo^.Next;

  while Current <> Limit do
  begin
    Previous^.Next := Current^.Next;
    Current^.Next^.Previous := Previous;

    Dispose(Current);
    Dec(fSize);
    Current := Previous^.Next;
  end;
end;

{--- TGenList.DeleteNodesForward ---}
procedure TGenList.DeleteNodesForward(From: PNode; Count: Integer);
var
  Current, BeforeFrom : PNode;
begin
  BeforeFrom := From^.Previous;
  Current := From;
  while (Count > 0) and (Current <> fTail) do
  begin
    BeforeFrom^.Next := Current^.Next;
    Current^.Next^.Previous := BeforeFrom;

    Dispose(Current);
    Dec(fSize);
    Dec(Count);
    Current := BeforeFrom^.Next;
  end;
end;

{--- TGenList.EnumeratorGet ---}
function TGenList.EnumeratorGet(const Pos: TListCursor): _TItem_;
begin
  ReadItemFast(Pos, Result);
end;

{--- TGenList.EnumeratorNext ---}
function TGenList.EnumeratorNext(var Pos: TListCursor): Boolean;
begin
  if Pos.IsNil then
    Pos := First
  else
    Pos.MoveNext;
  Result := Pos.HasItem;
end;

{--- TGenList.DeleteRange ---}
procedure TGenList.DeleteRange(const PosFrom, PosTo: TListCursor);
begin
  CheckNotNil(PosFrom);
  CheckNotNil(PosTo);
  DeleteNodesBetween(PosFrom.Node, PosTo.Node);
end;

{--- TGenList.Destroy ---}
destructor TGenList.Destroy;
begin
  Clear;
  Dispose(fHead);
  Dispose(fTail);
  inherited Destroy;
end;

{--- TGenList.Equals ---}
function TGenList.Equals(List: TGenList; Comparator: TCompareItems): Boolean;
var
  N1, N2 : PNode;
begin
  if fSize <> List.fSize then
  begin
    Result := false;
    Exit;
  end;

  Result := true;
  N1 := fHead^.Next;
  N2 := List.fHead^.Next;

  while N1 <> fTail do
  begin
    if Comparator(N1^.Item, N2^.Item) <> 0 then
    begin
      Result := false;
      Break;
    end;
    N1 := N1^.Next;
    N2 := N2^.Next;
  end;
end;

{--- TGenList.Equals ---}
function TGenList.Equals(Obj: TObject): Boolean;
begin
  Result := Equals(Obj, fOnCompareItems);
end;

{--- TGenList.Equals ---}
function TGenList.Equals(Obj: TObject; Comparator: TCompareItems): Boolean;
begin
  if Obj = Self  then
    Result := true
  else if Obj is TGenList then
    Result := Equals(Obj as TGenList, Comparator)
  else
    Result := false;
end;

{--- TGenList.Find ---}
function TGenList.Find(const Item: _TItem_): TListCursor;
begin
  Result := Find(Item, fOnCompareItems);
end;

{--- TGenList.Find ---}
function TGenList.Find(const Item: _TItem_; Comparator: TCompareItems): TListCursor;
begin
  if fSize = 0 then
    Result := fNilCursor
  else
    Result := Find(Item, First, Comparator);
end;

{--- TGenList.Find ---}
function TGenList.Find(const Item: _TItem_; const Position: TListCursor): TListCursor;
begin
  Result := Find(Item, Position, fOnCompareItems);
end;

{--- TGenList.Find ---}
function TGenList.Find(const Item: _TItem_; const Position: TListCursor; Comparator: TCompareItems): TListCursor;
var
  Node : PNode;
  I : _TItem_;
begin
  CheckValid(Position);

  if Position.IsNil then
    Node := fHead^.Next
  else
    Node := Position.Node;

  while Node <> fTail do
  begin
    I := Node^.Item;
    if Comparator(Item, I) = 0 then
      Break;
    Node := Node^.Next;
  end;

  if (Node = fTail) or (Node = fHead) then
    Node := nil;

  Result.Init(Self, Node);
end;

{--- TGenList.First ---}
function TGenList.First: TListCursor;
begin
  if fSize > 0 then
    Result.Init(Self, fHead^.Next)
  else
    Result := fNilCursor;
end;

{--- TGenList.FirstItem ---}
function TGenList.FirstItem: _TItem_;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := fHead^.Next^.Item;
end;

{--- TGenList.GetCursor ---}
function TGenList.GetCursor(Index: Integer): TListCursor;
var
  DistanceFromHead, DistanceFromTail : LongInt;
  Node : PNode;
begin
  if (Index < -1) or (Index > fSize) then
    Result := fNilCursor
  else
  begin
    DistanceFromHead := Index + 1;
    DistanceFromTail := fSize - Index;

    if DistanceFromHead < DistanceFromTail then
    begin
      Node := fHead;
      while DistanceFromHead > 0 do
      begin
        Node := Node^.Next;
        Dec(DistanceFromHead);
      end;
    end
    else
    begin
      Node := fTail;
      while DistanceFromTail > 0 do
      begin
        Node := Node^.Previous;
        Dec(DistanceFromTail);
      end;
    end;

    Result.Init(Self, Node);
  end;
end;

{--- TGenList.GetEnumerator ---}
function TGenList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(fNilCursor, @EnumeratorNext, @EnumeratorGet);
end;

{--- TGenList.GetItem ---}
function TGenList.GetItem(const Position: TListCursor): _TItem_;
begin
  CheckNotNil(Position);
  Result := PNode(Position.Node)^.Item;
end;

{--- TGenList.GetItemFast ---}
function TGenList.GetItemFast(const Position: TListCursor): _TItem_;
begin
  Result := PNode(Position.Node)^.Item;
end;

{--- TGenList.GetItemFast ---}
function TGenList.GetItemPtr(const Position: TListCursor): PItem;
begin
  CheckNotNil(Position);
  Result := @PNode(Position.Node)^.Item;
end;

{--- TGenList.GetItemFast ---}
function TGenList.GetItemPtrFast(const Position: TListCursor): PItem;
begin
  Result := @PNode(Position.Node)^.Item;
end;

{--- TGenList.Insert ---}
procedure TGenList.Insert(const Before: TListCursor; const Item: _TItem_;
  Count: Integer);
var
  BeforeNode : PNode;
begin
  CheckValid(Before);

  if Before.HasItem then
    BeforeNode := PNode(Before.Node)
  else
    BeforeNode := fTail;

  InsertItem(Item, BeforeNode, Count);
end;

{--- TGenList.Insert ---}
procedure TGenList.Insert(const Before: TListCursor; const Item: _TItem_;
  out Position: TListCursor; Count: Integer);
var
  Prev, BeforeNode : PNode;
begin
  CheckValid(Before);

  if Before.HasItem then
    BeforeNode := PNode(Before.Node)
  else
    BeforeNode := fTail;

  Prev := BeforeNode^.Previous;

  InsertItem(Item, BeforeNode, Count);

  Position.Init(Self, Prev^.Next);
end;

{--- TGenList.InsertAll ---}
procedure TGenList.InsertAll(const Before: TListCursor; Src: TGenList);
begin
  if Src.fSize > 0 then
    InsertRange(Before, Src, Src.First, Src.Last);
end;

{--- TGenList.InsertItem ---}
procedure TGenList.InsertItem(const Item: _TItem_; Pos: PNode; Count: Integer);
var
  Node : PNode;
begin
  while Count > 0 do
  begin
    New(Node);
    Node^.Item := Item;

    Pos^.Previous^.Next := Node;

    Node^.Previous := Pos^.Previous;
    Node^.Next := Pos;

    Pos^.Previous := Node;

    Inc(fSize);
    Dec(Count);
  end;
end;

{--- TGenList.InsertRange ---}
procedure TGenList.InsertRange(const Before : TListCursor; Src: TGenList;
  const PosFrom, PosTo: TListCursor);
var
  Copy: TGenList;
  Node, LastNode: PNode;
begin
  CheckValid(Before);
  Src.CheckNotNil(PosFrom);
  Src.CheckNotNil(PosTo);

  Copy := TGenList.Create;
  try    
    Node := PNode(PosFrom.Node);
    LastNode := PNode(PosTo.Node)^.Next;

    while Node <> LastNode do
    begin
      Copy.Append(Node^.Item);
      Node := Node^.Next;
    end;

    Splice(Before, Copy);
  finally
    Copy.Free;
  end;
end;

{--- TGenList.IsEmpty ---}
function TGenList.IsEmpty: Boolean;
begin
  Result := (fSize = 0);
end;

{--- TGenList.IsSorted ---}
function TGenList.IsSorted : Boolean;
begin
  Result := IsSorted(fOnCompareItems);
end;

{--- TGenList.IsSorted ---}
function TGenList.IsSorted(Comparator: TCompareItems) : Boolean;
var
  N : PNode;
  I : Integer;
begin
  Result := true;

  N := fHead^.Next;
  for I := 2 to fSize do
  begin
    if Comparator(N^.Item, N^.Next^.Item) > 0 then
    begin
      Result := false;
      Break;
    end;
    N := N^.Next;
  end;
end;

{--- TGenList.DefaultItemToString ---}
function TGenList.DefaultItemToString(const Item: _TItem_): String;
begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
end;

{--- TGenList.Iterate ---}
procedure TGenList.Iterate(Process: TProcessItem);
begin
  if fSize > 0 then
    Iterate(Process, First, Last);
end;

{--- TGenList.Iterate ---}
procedure TGenList.Iterate(Process: TProcessItem; const PosFrom, PosTo: TListCursor);
var
  Node, Limit : PNode;
begin
  CheckNotNil(PosFrom);
  CheckNotNil(PosTo);

  Node := PNode(PosFrom.Node);
  Limit := PNode(PosTo.Node)^.Next ;

  while Node <> Limit do
  begin
    Process(Node^.Item);
    Node := Node^.Next;
  end;
end;

{--- TGenList.Last ---}
function TGenList.Last: TListCursor;
begin
  if fSize > 0 then
    Result.Init(Self, fTail^.Previous)
  else
    Result.Init(Self, nil);
end;

{--- TGenList.LastItem ---}
function TGenList.LastItem: _TItem_;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := fTail^.Previous^.Item;
end;

{--- TGenList.Merge ---}
procedure TGenList.Merge(Src: TGenList);
begin
  Merge(Src, fOnCompareItems);
end;

{--- TGenList.Merge ---}
procedure TGenList.Merge(Src: TGenList; Comparator: TCompareItems);
var
  Node, SrcNode, N : PNode;
begin
  if Src = Self then
    Exit;

  Node := fHead^.Next;
  SrcNode := Src.fHead^.Next;

  while SrcNode <> Src.fTail do
  begin
    if Node = fTail then
    begin
      SpliceNodes(fTail, SrcNode, SrcNode);
      fSize := fSize + Src.fSize;
      Src.fSize := 0;
      Break;
    end;

    if Comparator(SrcNode^.Item, Node^.Item) < 0 then
    begin
      N := SrcNode^.Next;
      SpliceNodes(Node, SrcNode, SrcNode);
      Dec(Src.fSize);
      Inc(fSize);
      SrcNode := N;
    end
    else
      Node := Node^.Next;
  end;
end;

{--- TGenList.Partition ---}
procedure TGenList.Partition(Pivot, Back: PNode; Comparator: TCompareItems);
var
  Node, Next : PNode;
begin
  Node := Pivot^.Next;
  while Node <> Back do
    if Comparator(Node^.Item, Pivot^.Item) < 0 then
    begin
      Next := Node^.Next;
      SpliceNodes(Pivot, Node, Node);
      Node := Next;
    end
    else
      Node := Node^.Next;
end;

{--- TGenList.Prepend ---}
procedure TGenList.Prepend(const Item: _TItem_; Count: Integer);
begin
  Insert(First, Item, Count);
end;

{--- TGenList.PrependAll ---}
procedure TGenList.PrependAll(Src: TGenList);
begin
  InsertAll(First, Src);
end;

{--- TGenList.PrependRange ---}
procedure TGenList.PrependRange(Src: TGenList; const PosFrom, PosTo: TListCursor);
begin
  InsertRange(First, Src, PosFrom, PosTo);
end;

{--- TGenList.ReadFirstItem ---}
procedure TGenList.ReadFirstItem(out Value : _TItem_);
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Value := fHead^.Next^.Item;
end;

{--- TGenList.ReadItem ---}
procedure TGenList.ReadItem(const Position: TListCursor; out Value: _TItem_);
begin
  CheckNotNil(Position);
  Value := PNode(Position.Node)^.Item;
end;

{--- TGenList.ReadItemFast ---}
procedure TGenList.ReadItemFast(const Position: TListCursor; out Value: _TItem_);
begin
  Value := PNode(Position.Node)^.Item;
end;

{--- TGenList.ReadLastItem ---}
procedure TGenList.ReadLastItem(out Value : _TItem_);
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Value := fTail^.Previous^.Item;
end;

{--- TGenList.RealSort ---}
procedure TGenList.RealSort(Front, Back: PNode; Comparator: TCompareItems);
var
  Pivot : PNode;
begin
  Pivot := Front^.Next;
  if Pivot <> Back then
  begin
     Partition(Pivot, Back, Comparator);
     RealSort(Front, Pivot, Comparator);
     RealSort(Pivot, Back, Comparator)
  end;
end;

{--- TGenList.SetOnCompareItems ---}
procedure TGenList.SetOnCompareItems(AValue: TCompareItems);
begin
  if AValue = nil then
    fOnCompareItems := @DefaultCompareItems
  else
    fOnCompareItems := AValue;
end;

{--- TGenList.SetOnItemToString ---}
procedure TGenList.SetOnItemToString(AValue: TItemToString);
begin
  if AValue = nil then
    fOnItemToString := @DefaultItemToString
  else
    fOnItemToString := AValue;
end;

{--- TGenList.Replace ---}
procedure TGenList.Replace(const Position: TListCursor; Count: Integer;
  const Value: _TItem_);
var
  Node : PNode;
begin
  CheckNotNil(Position);

  Node := PNode(Position.Node);
  while (Count > 0) and (Node <> fTail) do
  begin
    Node^.Item := Value;
    Dec(Count);
    Node := Node^.Next;
  end;
end;

{--- TGenList.Reverse ---}
procedure TGenList.Reverse;
begin
  if fSize > 1 then
    ReverseRange(First, Last);
end;

{--- TGenList.ReverseFind ---}
function TGenList.ReverseFind(const Item: _TItem_): TListCursor;
begin
  Result := ReverseFind(Item, fOnCompareItems);
end;

{--- TGenList.ReverseFind ---}
function TGenList.ReverseFind(const Item: _TItem_; Comparator: TCompareItems): TListCursor;
begin
  if fSize = 0 then
    Result := fNilCursor
  else
    Result := ReverseFind(Item, Last, Comparator);
end;

{--- TGenList.ReverseFind ---}
function TGenList.ReverseFind(const Item: _TItem_; const Position: TListCursor): TListCursor;
begin
  Result := ReverseFind(Item, Position, fOnCompareItems);
end;

{--- TGenList.ReverseFind ---}
function TGenList.ReverseFind(const Item: _TItem_;
  const Position: TListCursor; Comparator: TCompareItems): TListCursor;
var
  Node : PNode;
  I : _TItem_;
begin
  CheckValid(Position);

  if Position.IsNil then
    Node := fTail^.Previous
  else
    Node := PNode(Position.Node);

  if Node = fTail then
    Node := Node^.Previous;

  while Node <> fHead do
  begin
    I := Node^.Item;
    if Comparator(Item, I) = 0 then
      Break;
    Node := Node^.Previous;
  end;

  if (Node = fTail) or (Node = fHead) then
    Node := nil;

  Result.Init(Self, Node);
end;

{--- TGenList.ReverseRange ---}
procedure TGenList.ReverseRange(const PosFrom, PosTo: TListCursor);
var
  Left, Right : PNode;
  Tmp : _TItem_;
begin
  CheckNotNil(PosFrom);
  CheckNotNil(PosTo);

  if not PosFrom.Equals(PosTo) then
  begin
    Left := PNode(PosFrom.Node);
    Right := PNode(PosTo.Node);
    while true do
    begin
      Tmp := Left^.Item;
      Left^.Item := Right^.Item;
      Right^.Item := Tmp;

      Left := Left^.Next;
      if Left = Right then
        Break;

      Right := Right^.Previous;
      if Left = Right then
        Break;
    end;
  end;
end;

{--- TGenList.SetItem ---}
procedure TGenList.SetItem(const Position: TListCursor; const Value: _TItem_);
begin
  CheckNotNil(Position);
  PNode(Position.Node)^.Item := Value;
end;

{--- TGenList.SetItemFast ---}
procedure TGenList.SetItemFast(const Position: TListCursor; const Value: _TItem_);
begin
  PNode(Position.Node)^.Item := Value;
end;

{--- TGenList.Sort ---}
procedure TGenList.Sort(const PosFrom, PosTo: TListCursor);
begin
  Sort(PosFrom, PosTo, fOnCompareItems);
end;

{--- TGenList.Sort ---}
procedure TGenList.Sort(const PosFrom, PosTo: TListCursor; Comparator: TCompareItems);
begin
  CheckNotNil(PosFrom);
  CheckNotNil(PosTo);
  RealSort(PNode(PosFrom.Node)^.Previous, PNode(PosTo.Node)^.Next, Comparator);
end;

{--- TGenList.Sort ---}
procedure TGenList.Sort;
begin
  Sort(fOnCompareItems);
end;

{--- TGenList.Sort ---}
procedure TGenList.Sort(Comparator: TCompareItems);
begin
  if fSize > 1 then
    Sort(First, Last, Comparator);
end;

{--- TGenList.Splice ---}
procedure TGenList.Splice(const Before: TListCursor; Src: TGenList);
var
  Where : PNode;
begin
  CheckValid(Before);

  if (Self <> Src) and (Src.fSize > 0) then
  begin
    if Before.IsNil then
      Where := fTail
    else
      Where := PNode(Before.Node);

    SpliceNodes(Where, Src.fHead^.Next, Src.fTail^.Previous);
    Inc(fSize, Src.fSize);
    Src.fSize:=0;
  end;
end;

{--- TGenList.Splice ---}
procedure TGenList.Splice(const Before: TListCursor; Src: TGenList;
  const SrcFrom, SrcTo: TListCursor);
var
  Node, Where : PNode;
  Count : Integer = 0;
begin
  CheckValid(Before);
  Src.CheckNotNil(SrcFrom);
  Src.CheckNotNil(SrcTo);

  if (Src = Self) and Before.HasItem then
  begin
    if Before.Equals(SrcFrom) or Before.Equals(SrcTo) then
      RaiseError('cursor `Before'' is in range [SrcFrom..SrcTo]');

    Node := PNode(SrcFrom.Node)^.Next;
    while Node <> PNode(SrcTo.Node) do
    begin
      if Node = PNode(Before.Node) then
        RaiseError('cursor `Before'' is in range [SrcFrom..SrcTo]');
      Node := Node^.Next;
    end;
  end
  else if Src <> Self then
  begin
    Node := PNode(SrcFrom.Node);
    while Node <> PNode(SrcTo.Node) do
    begin
      Node := Node^.Next;
      Inc(Count);
    end;
    Inc(Count);
  end;

  if Before.HasItem then
    Where := PNode(Before.Node)
  else
    Where := fTail;

  SpliceNodes(Where, PNode(SrcFrom.Node), PNode(SrcTo.Node));
  Inc(fSize, Count);
  Dec(Src.fSize, Count);
end;

{--- TGenList.Splice ---}
procedure TGenList.Splice(const Before: TListCursor; Src: TGenList;
  const Position: TListCursor);
var
  Where : PNode;
begin
  CheckValid(Before);
  Src.CheckNotNil(Position);

  if not Position.Equals(Before) then
  begin
    if Before.HasItem then
      Where := PNode(Before.Node)
    else
      Where := fTail;

    SpliceNodes(Where, PNode(Position.Node), PNode(Position.Node));
    Inc(fSize);
    Dec(Src.fSize);
  end;
end;

{--- TGenList.SpliceNodes ---}
procedure TGenList.SpliceNodes(Before, PosFrom, PosTo: PNode);
begin
  PosFrom^.Previous^.Next := PosTo^.Next;
  PosTo^.Next^.Previous := PosFrom^.Previous;

  Before^.Previous^.Next := PosFrom;
  PosFrom^.Previous := Before^.Previous;

  PosTo^.Next := Before;
  Before^.Previous := PosTo;
end;

{--- TGenList.CursorIsFirst ---}
function TGenList.CursorIsFirst(const Cursor: TListCursor): Boolean;
begin
  Result := (PNode(Cursor.Node) = (Cursor.List as TGenList).fHead^.Next) and
            (PNode(Cursor.Node) <> (Cursor.List as TGenList).fTail);
end;

{--- TGenList.CursorIsLast ---}
function TGenList.CursorIsLast(const Cursor: TListCursor): Boolean;
begin
  Result := (PNode(Cursor.Node) = (Cursor.List as TGenList).fTail^.Previous) and
            (PNode(Cursor.Node) <> (Cursor.List as TGenList).fHead);
end;

{--- TGenList.CursorMoveNext ---}
procedure TGenList.CursorMoveNext(var Cursor: TListCursor);
begin
  if Cursor.Node <> nil then
  begin
    Cursor.Node := PNode(Cursor.Node)^.Next;
    if PNode(Cursor.Node) = (Cursor.List as TGenList).fTail then
      Cursor.Node := nil;
  end;
end;

{--- TGenList.CursorMovePrev ---}
procedure TGenList.CursorMovePrev(var Cursor: TListCursor);
begin
  if Cursor.Node <> nil then
  begin
    Cursor.Node := PNode(Cursor.Node)^.Previous;
    if PNode(Cursor.Node) = (Cursor.List as TGenList).fHead then
      Cursor.Node := nil;
  end;
end;

{--- TGenList.Swap ---}
procedure TGenList.Swap(const I, J: TListCursor);
var
  Tmp : _TItem_;
begin
  CheckNotNil(I);
  CheckNotNil(J);

  if I.Node <> J.Node then
  begin
    Tmp := PNode(I.Node)^.Item;
    PNode(I.Node)^.Item := PNode(J.Node)^.Item;
    PNode(J.Node)^.Item := Tmp;
  end;
end;

{--- TGenList.SwapLinks ---}
procedure TGenList.SwapLinks(const I, J: TListCursor);
var
  NextI : PNode;
begin
  CheckNotNil(I);
  CheckNotNil(J);

  if I.Node <> J.Node then
  begin
    NextI := PNode(I.Node)^.Next;

    if NextI = PNode(J.Node) then
      SpliceNodes(PNode(I.Node), PNode(J.Node), PNode(J.Node))
    else
    begin
      SpliceNodes(PNode(J.Node), PNode(I.Node), PNode(I.Node));
      SpliceNodes(NextI, PNode(J.Node), PNode(J.Node) );
    end;
  end;
end;

{--- TGenList.ToString ---}
function TGenList.ToString: String;
var
  Node : PNode;
begin
  Result := '(';

  if fSize > 0 then
  begin
    Node := fHead^.Next;
    while Node <> fTail do
    begin
      Result := Result + fOnItemToString(Node^.Item) + ', ';
      Node := Node^.Next;
    end;
    SetLength(Result, Length(Result) - 2);
  end;

  Result := Result + ')';
end;

{=========================}
{=== TGenPriorityQueue ===}
{=========================}

{--- TGenPriorityQueue.Clear ---}
procedure TGenPriorityQueue.Clear;
begin
  SetLength(fItems, 1);
  fCapacity := 1;
  fSize := 0;
end;

{--- TGenPriorityQueue.Create ---}
constructor TGenPriorityQueue.Create(InitialCapacity : Integer);
begin
  inherited Create;

  if InitialCapacity < 1 then
    InitialCapacity := 1;

  SetLength(fItems, InitialCapacity);
  fCapacity := InitialCapacity;

  fSize := 0;

  SetOnCompareItems(nil);
end;

{--- TGenPriorityQueue.DefaultCompareItems ---}
function TGenPriorityQueue.DefaultCompareItems(const A, B: _TItem_): Integer;
begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := 0;
end;

{--- TGenPriorityQueue.IsEmpty ---}
function TGenPriorityQueue.IsEmpty: Boolean;
begin
  Result := (fSize = 0);
end;

{--- TGenPriorityQueue.Pack ---}
procedure TGenPriorityQueue.Pack;
begin
  SetLength(fItems, fSize);
  fCapacity := fSize;
end;

{--- TGenPriorityQueue.Pop ---}
procedure TGenPriorityQueue.Pop;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Dec(fSize);
  if fSize > 0 then
    MoveDown(0, fItems[fSize]);
end;

{--- TGenPriorityQueue.Push ---}
procedure TGenPriorityQueue.Push(const Item: _TItem_);
begin
  if fSize = fCapacity then
    Reserve(fSize + 1);

  if fSize = 0 then
    fItems[0] := Item
  else
    MoveUp(fSize, Item);

  Inc(fSize);
end;

{--- TGenPriorityQueue.ReadTop ---}
procedure TGenPriorityQueue.ReadTop(out Value: _TItem_);
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Value := fItems[0];
end;

{--- TGenPriorityQueue.Reserve ---}
procedure TGenPriorityQueue.Reserve(MinCapacity: Integer);
var
  NewCapacity : Integer;
begin
  if MinCapacity > fCapacity then
  begin
    if fCapacity <= 128 then
      NewCapacity := fCapacity *  2
    else
      NewCapacity := (fCapacity * 3) div 2;

    if NewCapacity < MinCapacity then
      NewCapacity := MinCapacity;

    SetLength(fItems, NewCapacity);
    fCapacity := NewCapacity;
  end;
end;

{--- TGenPriorityQueue.MoveDown ---}
procedure TGenPriorityQueue.MoveDown(Index: Integer; const Item: _TItem_);
var
  Half, Child, Right : Integer;
begin
  Half := fSize shr 1;

  while Index < Half do
  begin
    Child := (Index shl 1) + 1;

    Right := Child + 1;

    if (Right < fSize) and
       (fOnCompareItems(fItems[Child], fItems[Right]) > 0) then
      Child := Right;

    if fOnCompareItems(Item, fItems[Child]) <= 0 then
      Break;

    fItems[Index] := fItems[Child];
    Index := Child;
  end;
  fItems[Index] := Item;
end;

{--- TGenPriorityQueue.SetOnCompareItems ---}
procedure TGenPriorityQueue.SetOnCompareItems(AValue: TCompareItems);
begin
  if AValue = nil then
    fOnCompareItems := @DefaultCompareItems
  else
    fOnCompareItems := AValue;
end;

{--- TGenPriorityQueue.MoveUp ---}
procedure TGenPriorityQueue.MoveUp(Index: Integer; const Item: _TItem_);
var
  Parent : Integer;
begin
  while Index > 0 do
  begin
    Parent := (Index - 1) shr 1;

    if fOnCompareItems(Item, fItems[Parent]) >= 0 then
      Break;

    fItems[Index] := fItems[Parent];
    Index := Parent;
  end;
  fItems[Index] := Item;
end;

{--- TGenPriorityQueue.Top ---}
function TGenPriorityQueue.Top: _TItem_;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := fItems[0];
end;

{=================}
{=== TGenQueue ===}
{=================}

{--- TGenQueue.Append ---}
procedure TGenQueue.Append(const Item: _TItem_);
begin
  fData.Append(Item);
end;

{--- TGenQueue.Clear ---}
procedure TGenQueue.Clear;
begin
  fData.Clear;
end;

{--- TGenQueue.Create ---}
constructor TGenQueue.Create;
begin
  inherited Create;
  fData := _TContainer_.Create;
end;

{--- TGenQueue.Destroy ---}
destructor TGenQueue.Destroy;
begin
  fData.Free;
  inherited Destroy;
end;

{--- TGenQueue.Front ---}
function TGenQueue.Front: _TItem_;
begin
  fData.ReadFirstItem(Result);
end;

{--- TGenQueue.GetSize ---}
function TGenQueue.GetSize: Integer;
begin
  Result := fData.Size;
end;

{--- TGenQueue.IsEmpty ---}
function TGenQueue.IsEmpty: Boolean;
begin
  Result := fData.Size = 0;
end;

{--- TGenQueue.Pop ---}
procedure TGenQueue.Pop;
begin
  fData.DeleteFirst;
end;

{--- TGenQueue.ReadFront ---}
procedure TGenQueue.ReadFront(out Value: _TItem_);
begin
  fData.ReadFirstItem(Value);
end;

{=================}
{=== TGenStack ===}
{=================}

{--- TGenStack.Clear ---}
procedure TGenStack.Clear;
begin
  fData.Clear;
end;

{--- TGenStack.Create ---}
constructor TGenStack.Create;
begin
  inherited Create;
  fData := _TContainer_.Create;
end;

{--- TGenStack.Destroy ---}
destructor TGenStack.Destroy;
begin
  fData.Free;
  inherited Destroy;
end;

{--- TGenStack.GetSize ---}
function TGenStack.GetSize: Integer;
begin
  Result := fData.Size;
end;

{--- TGenStack.IsEmpty ---}
function TGenStack.IsEmpty: Boolean;
begin
  Result := (fData.Size = 0);
end;

{--- TGenStack.Pop ---}
procedure TGenStack.Pop;
begin
  fData.DeleteLast;
end;

{--- TGenStack.Push ---}
procedure TGenStack.Push(const Item: _TItem_);
begin
  fData.Append(Item);
end;

{--- TGenStack.ReadTop ---}
procedure TGenStack.ReadTop(out Value : _TItem_);
begin
  fData.ReadLastItem(Value);
end;

{--- TGenStack.Top ---}
function TGenStack.Top: _TItem_;
begin
  fData.ReadLastItem(Result);
end;

{======================}
{=== THashMapCursor ===}
{======================}

{--- THashMapCursor.Equals ---}
function THashMapCursor.Equals(const Cursor: THashMapCursor): Boolean;
begin
  Result := (fHashMap = Cursor.fHashMap) and (fBucket = Cursor.fBucket)
            and (fEntry = Cursor.fEntry);
end;

{--- THashMapCursor.HasItem ---}
function THashMapCursor.HasItem: Boolean;
begin
  Result := (fEntry <> nil);
end;

{--- THashMapCursor.Init ---}
constructor THashMapCursor.Init(HashMap: TAbstractHashMap; BucketNum: Integer;
  AEntry, APrevious: Pointer);
begin
  fHashMap := HashMap;
  fBucket := BucketNum;
  fEntry := AEntry;
  fPrevious := APrevious;
end;

{--- THashMapCursor.IsFirst ---}
function THashMapCursor.IsFirst: Boolean;
begin
  Result := fHashMap.CursorIsFirst(Self);
end;

{--- THashMapCursor.IsLast ---}
function THashMapCursor.IsLast: Boolean;
begin
  Result := fHashMap.CursorIsLast(Self);
end;

{--- THashMapCursor.IsNil ---}
function THashMapCursor.IsNil: Boolean;
begin
  Result := (fEntry = nil);
end;

{--- THashMapCursor.MoveNext ---}
procedure THashMapCursor.MoveNext;
begin
  fHashMap.CursorMoveNext(Self);
end;

{===================}
{=== TGenHashMap ===}
{===================}

{--- TGenHashMap.AppendBuckets ---}
procedure TGenHashMap.AppendBuckets(Count: Integer);
begin
  if Count > 0 then
  begin
    ReallocMem(fBuckets, SizeOf(PEntry) * (fBucketCount + Count));
    NilifyBuckets(fBucketCount, Count);
    fBucketCount := fBucketCount + Count;
  end;
end;

{--- TGenHashMap.CollectEntries ---}
function TGenHashMap.CollectEntries: PEntry;
var
  I : Integer;
  FirstEntry, LastEntry : PEntry;
begin
  Result := nil;

  for I := 0 to fBucketCount - 1 do
  begin
    FirstEntry := fBuckets[I];

    if FirstEntry <> nil then
    begin
      LastEntry := FirstEntry;
      while LastEntry^.Next <> nil do
        LastEntry := LastEntry^.Next;

      LastEntry^.Next := Result;
      Result := FirstEntry;
    end;
  end;
end;

{--- TGenHashMap.Clear ---}
procedure TGenHashMap.Clear;
var
  I : Integer;
begin
  for I := 0 to fBucketCount - 1 do
  begin
    if fBuckets[I] <> nil then
    begin
      DisposeEntries(fBuckets[I]);
      fBuckets[I] := nil;
    end;
  end;

  fSize := 0;
  fFirstNonEmptyBucket := -1;
  fLastNonEmptyBucket := -1;
end;

{--- TGenHashMap.Contains ---}
function TGenHashMap.Contains(const Key: _TKey_): Boolean;
begin
  Result := GetEntry(Key) <> nil;
end;

{--- TGenHashMap.Create ---}
constructor TGenHashMap.Create(InitialCapacity: Integer);
begin
  Create(InitialCapacity, DEFAULT_HASHMAP_LOAD_FACTOR)
end;

{--- TGenHashMap.Create ---}
constructor TGenHashMap.Create(InitialCapacity: Integer; MaxLoadFact: Real);
var
  Capacity : Integer;
begin
  inherited Create;

  if InitialCapacity <= 0 then
    InitialCapacity := MIN_BUCKET_COUNT;

  if InitialCapacity > MAX_BUCKET_COUNT then
    InitialCapacity := MAX_BUCKET_COUNT;

  if MaxLoadFact <= 0 then
    MaxLoadFact := DEFAULT_HASHMAP_LOAD_FACTOR;

  Capacity := MIN_BUCKET_COUNT;
  while Capacity < InitialCapacity do
    Capacity := Capacity * 2;

  fSize := 0;

  fMaxLoadFactor := MaxLoadFact;
  fThreshold := Round(Capacity * MaxLoadFact);

  fMaxBucketCount := MAX_BUCKET_COUNT;

  fBuckets := nil;
  fBucketCount := 0;
  AppendBuckets(Capacity);

  fFirstNonEmptyBucket := -1;
  fLastNonEmptyBucket := -1;

  fNilCursor.Init(Self, -1, nil, nil);

  SetOnHashKey(nil);
  SetOnItemToString(nil);
  SetOnKeysEqual(nil);
  SetOnKeyToString(nil);
end;

{--- TGenHashMap.Create ---}
constructor TGenHashMap.Create(MaxLoadFact: Real);
begin
  Create(MIN_BUCKET_COUNT, MaxLoadFact);
end;

{--- TGenHashMap.Delete ---}
procedure TGenHashMap.Delete(const Key: _TKey_);
var
  Bucket: Integer;
  Entry, Previous : PEntry;
begin
  Bucket := IndexFor(fOnHashKey(Key));
  Entry := FindEntry(Bucket, Key, Previous);

  if Entry = nil then
    RaiseKeyNotInMap
  else
    DeleteEntry(Bucket, Entry, Previous);
end;

{--- TGenHashMap.DeleteAt ---}
procedure TGenHashMap.DeleteAt(const Position: THashMapCursor);
begin
  if Position.HashMap <> Self then
    RaiseCursorDenotesWrongContainer;

  if Position.IsNil then
    RaiseCursorIsNil;

  DeleteEntry(Position.Bucket, Position.Entry, Position.Previous)
end;

{--- TGenHashMap.DeleteEntry ---}
procedure TGenHashMap.DeleteEntry(Bucket: Integer; Entry, Previous: PEntry);
var
  Next : PEntry;
begin
  Next := Entry^.Next;

  if Previous <> nil then
    Previous^.Next := Next;

  if fBuckets[Bucket] = Entry then
  begin
    fBuckets[Bucket] := Next;

    if Next = nil then
    begin
      if Bucket = fFirstNonEmptyBucket then
        fFirstNonEmptyBucket := NextNonEmptyBucket(Bucket + 1);

      if Bucket = fLastNonEmptyBucket then
        fLastNonEmptyBucket := PreviousNonEmptyBucket(Bucket - 1);
    end;
  end;

  Dispose(Entry);
  Dec(fSize);
end;

{--- TGenHashMap.DisposeEntries ---}
procedure TGenHashMap.DisposeEntries(E: PEntry);
var
  N: PEntry;
begin
  while E <> nil do
  begin
    N := E^.Next;
    Dispose(E);
    E := N;
  end;
end;

{--- TGenHashMap.EnumeratorGet ---}
function TGenHashMap.EnumeratorGet(const Pos: THashMapCursor): _TItem_;
begin
  ReadItemAt(Pos, Result);
end;

{--- TGenHashMap.EnumeratorNext ---}
function TGenHashMap.EnumeratorNext(var Pos: THashMapCursor): Boolean;
begin
  if Pos.IsNil then
    Pos := First
  else
    Pos.MoveNext;
  Result := Pos.HasItem;
end;

{--- TGenHashMap.Destroy ---}
destructor TGenHashMap.Destroy;
begin
  Clear;
  FreeMem(fBuckets);
  inherited Destroy;
end;

{--- TGenHashMap.Exclude ---}
procedure TGenHashMap.Exclude(const Key: _TKey_);
var
  Bucket : Integer;
  Entry, Previous : PEntry;
begin
  Bucket := IndexFor(fOnHashKey(Key));
  Entry := FindEntry(Bucket, Key, Previous);

  if Entry <> nil then
    DeleteEntry(Bucket, Entry, Previous);
end;

{--- TGenHashMap.Find ---}
function TGenHashMap.Find(const Key: _TKey_): THashMapCursor;
var
  Bucket : Integer;
  Entry, Previous : PEntry;
begin
  Bucket := IndexFor(fOnHashKey(Key));

  Entry := FindEntry(Bucket, Key, Previous);

  Result.Init(Self, Bucket, Entry, Previous);
end;

{--- TGenHashMap.FindEntry ---}
function TGenHashMap.FindEntry(Bucket: Integer; const Key: _TKey_): PEntry;
begin
  Result := fBuckets[Bucket];
  while Result <> nil do
  begin
    if fOnKeysEqual(Result^.Key, Key) then
      Break;
    Result := Result^.Next;
  end;
end;

{--- TGenHashMap.FindEntry ---}
function TGenHashMap.FindEntry(Bucket: Integer; const Key: _TKey_; out Previous: PEntry) : PEntry;
begin
  Previous := nil;
  Result := fBuckets[Bucket];
  while Result <> nil do
  begin
    if fOnKeysEqual(Result^.Key, Key) then
      Break;
    Previous := Result;
    Result := Result^.Next;
  end;
end;

{--- TGenHashMap.First ---}
function TGenHashMap.First: THashMapCursor;
begin
  if fSize > 0 then
    Result.Init(Self, fFirstNonEmptyBucket, fBuckets[fFirstNonEmptyBucket], nil)
  else
    Result := fNilCursor;
end;

{--- TGenHashMap.GetEnumerator ---}
function TGenHashMap.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(fNilCursor, @EnumeratorNext, @EnumeratorGet);
end;

{--- TGenHashMap.GetEntry ---}
function TGenHashMap.GetEntry(const Key: _TKey_): PEntry;
begin
  Result := FindEntry( IndexFor(fOnHashKey(Key)), Key);
end;

{--- TGenHashMap.GetEntryAt ---}
function TGenHashMap.GetEntryAt(const Position: THashMapCursor): PEntry;
begin
  if Position.HashMap <> Self then
    RaiseCursorDenotesWrongContainer;

  if Position.IsNil then
    RaiseCursorIsNil;

  Result := Position.Entry;
end;

{--- TGenHashMap.GetItem ---}
function TGenHashMap.GetItem(const Key: _TKey_): _TItem_;
var
  Entry : PEntry;
begin
  Entry := GetEntry(Key);

  if Entry = nil then
    RaiseKeyNotInMap
  else
    Result := Entry^.Value;
end;

{--- TGenHashMap.GetItemAt ---}
function TGenHashMap.GetItemAt(const Position: THashMapCursor): _TItem_;
begin
  Result := GetEntryAt(Position)^.Value;
end;

{--- TGenHashMap.GetKeyAt ---}
function TGenHashMap.GetKeyAt(const Position: THashMapCursor): _TKey_;
begin
  Result := GetEntryAt(Position)^.Key;
end;

{--- TGenHashMap.DefaultHashKey ---}
function TGenHashMap.DefaultHashKey(const Key: _TKey_): Integer;
begin
  Unused(@Key);
  RaiseMethodNotRedefined;
  Result := 0;
end;

{--- TGenHashMap.GetLoadFactor ---}
function TGenHashMap.GetLoadFactor: Real;
begin
  Result := fSize / fBucketCount;
end;

{--- TGenHashMap.Include ---}
procedure TGenHashMap.Include(const Key: _TKey_; const Value: _TItem_);
var
  Hash, Bucket : Integer;
  Entry, Previous : PEntry;
begin
  Hash := fOnHashKey(Key);
  Bucket := IndexFor(Hash);

  Entry := FindEntry(Bucket, Key, Previous);
  if Entry <> nil then
    Entry^.Value := Value
  else
  begin
    Entry := NewEntry(Key, Value);

    if Previous <> nil then
      InsertEntry(Entry, Previous)
    else
      InsertEntry(Bucket, Entry);
  end;
end;

{--- TGenHashMap.IndexFor ---}
function TGenHashMap.IndexFor(Hash: Integer): Integer;
begin
  Result := LongWord(Hash) and (fBucketCount - 1);
end;

{--- TGenHashMap.InsertCollectedEntries ---}
procedure TGenHashMap.InsertCollectedEntries(CollectedEntries: PEntry);
var
  Entry, NextEntry : PEntry;
  Bucket : Integer;
begin
  Entry := CollectedEntries;
  while Entry <> nil do
  begin
    NextEntry := Entry^.Next;

    Bucket := IndexFor(fOnHashKey(Entry^.Key));
    Entry^.Next := fBuckets[Bucket];
    fBuckets[Bucket] := Entry;

    Entry := NextEntry;
  end;
end;

{--- TGenHashMap.Insert ---}
procedure TGenHashMap.Insert(const Key: _TKey_; const Value: _TItem_);
var
  Inserted : Boolean;
begin
  Insert(Key, Value, Inserted);
  if not Inserted then
    RaiseKeyAlreadyInMap;
end;

{--- TGenHashMap.Insert ---}
procedure TGenHashMap.Insert(const Key: _TKey_; const Value: _TItem_; out
  Inserted: Boolean);
var
  Hash, Bucket : Integer;
  Entry, Previous : PEntry;
begin
  Hash := fOnHashKey(Key);
  Bucket := IndexFor(Hash);

  Entry := FindEntry(Bucket, Key, Previous);

  if Entry <> nil then
    Inserted := false
  else
  begin
    Entry := NewEntry(Key, Value);

    if Previous <> nil then
      InsertEntry(Entry, Previous)
    else
      InsertEntry(Bucket, Entry);

    Inserted := true;
  end;
end;

{--- TGenHashMap.InsertEntry ---}
procedure TGenHashMap.InsertEntry(Bucket: Integer; Entry: PEntry);
begin
  Entry^.Next := fBuckets[Bucket];
  fBuckets[Bucket] := Entry;

  if (fFirstNonEmptyBucket = -1) or (Bucket < fFirstNonEmptyBucket) then
    fFirstNonEmptyBucket := Bucket;

  if (fLastNonEmptyBucket = -1) or (Bucket > fLastNonEmptyBucket) then
    fLastNonEmptyBucket := Bucket;

  Inc(fSize);
  if fSize > fThreshold then
    Resize(2 * fBucketCount);
end;

{--- TGenHashMap.InsertEntry ---}
procedure TGenHashMap.InsertEntry(Entry, Before: PEntry);
begin
  Before^.Next := Entry;
  Entry^.Next := nil;

  Inc(fSize);
  if fSize > fThreshold then
    Resize(2 * fBucketCount);
end;

{--- TGenHashMap.IsEmpty ---}
function TGenHashMap.IsEmpty: Boolean;
begin
  Result := (fSize = 0);
end;

{--- TGenHashMap.DefaultItemToString ---}
function TGenHashMap.DefaultItemToString(const Item: _TItem_): String;
begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
end;

{--- TGenHashMap.DefaultKeysEqual ---}
function TGenHashMap.DefaultKeysEqual(const A, B: _TKey_): Boolean;
begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := false;
end;

{--- TGenHashMap.DefaultKeyToString ---}
function TGenHashMap.DefaultKeyToString(const Key: _TKey_): String;
begin
  Unused(@Key);
  RaiseMethodNotRedefined;
  Result := '';
end;

{--- TGenHashMap.NextNonEmptyBucket ---}
function TGenHashMap.NextNonEmptyBucket(Bucket: Integer): Integer;
var
  I : Integer;
begin
  Result := -1;
  for I := Bucket to fBucketCount - 1 do
    if fBuckets[I] <> nil then
    begin
      Result := I;
      Exit;
    end;
end;

{--- TGenHashMap.NewEntry ---}
function TGenHashMap.NewEntry(const Key: _TKey_; const Value: _TItem_) : PEntry;
begin
  New(Result);
  Result^.Key := Key;
  Result^.Value := Value;
end;

{--- TGenHashMap.NilifyBuckets ---}
procedure TGenHashMap.NilifyBuckets(BucketFrom, Count: Integer);
var
  I : Integer;
begin
  for I := BucketFrom to BucketFrom + Count - 1 do
    fBuckets[I] := nil;
end;

{--- TGenHashMap.PreviousNonEmptyBucket ---}
function TGenHashMap.PreviousNonEmptyBucket(Bucket: Integer): Integer;
var
  I : Integer;
begin
  Result := -1;
  for I := Bucket downto 0 do
    if fBuckets[I] <> nil then
    begin
      Result := I;
      Break;
    end;
end;

{--- TGenHashMap.ReadItem ---}
procedure TGenHashMap.ReadItem(const Key: _TKey_; out Value: _TItem_);
var
  Entry : PEntry;
begin
  Entry := GetEntry(Key);

  if Entry = nil then
    RaiseKeyNotInMap
  else
    Value := Entry^.Value;
end;

{--- TGenHashMap.ReadItemAt ---}
procedure TGenHashMap.ReadItemAt(const Position: THashMapCursor; out Value: _TItem_);
begin
  Value := GetEntryAt(Position)^.Value;
end;

{--- TGenHashMap.ReadKeyAt ---}
procedure TGenHashMap.ReadKeyAt(const Position : THashMapCursor; out Key: _TKey_);
begin
  Key := GetEntryAt(Position)^.Key;
end;

{--- TGenHashMap.Replace ---}
procedure TGenHashMap.Replace(const Key: _TKey_; const Value: _TItem_);
var
  Bucket : Integer;
  Entry : PEntry;
begin
  Bucket := IndexFor(fOnHashKey(Key));

  Entry := FindEntry(Bucket, Key);

  if Entry = nil then
    RaiseKeyNotInMap;

  Entry^.Value := Value;
end;

{--- TGenHashMap.Resize ---}
procedure TGenHashMap.Resize(NewCapacity: Integer);
var
  CollectedEntries : PEntry;
  OldCapacity : Integer;
begin
  OldCapacity := fBucketCount;

  if OldCapacity = MAX_BUCKET_COUNT then
  begin
    fThreshold := High(Integer);
    Exit;
  end;

  { Collect all entries }
  CollectedEntries := CollectEntries;

  if (fFirstNonEmptyBucket >= 0) and (fLastNonEmptyBucket >= 0) then
    NilifyBuckets(fFirstNonEmptyBucket, fLastNonEmptyBucket - fFirstNonEmptyBucket + 1)
  else
    NilifyBuckets(0, fBucketCount);

  { Create necessary buckets }
  AppendBuckets(NewCapacity - OldCapacity);
  fThreshold := Round(NewCapacity * fMaxLoadFactor);

  { Re-insert collected entries }
  InsertCollectedEntries(CollectedEntries);

  fFirstNonEmptyBucket := NextNonEmptyBucket(0);
  fLastNonEmptyBucket := PreviousNonEmptyBucket(fBucketCount - 1);
end;

{--- TGenHashMap.SetOnHashKey ---}
procedure TGenHashMap.SetOnHashKey(AValue: THashKey);
begin
  if AValue = nil then
    fOnHashKey := @DefaultHashKey
  else
    fOnHashKey:=AValue;
end;

{--- TGenHashMap.SetOnItemToString ---}
procedure TGenHashMap.SetOnItemToString(AValue: TItemToString);
begin
  if AValue = nil then
    fOnItemToString := @DefaultItemToString
  else
    fOnItemToString := AValue;
end;

{--- TGenHashMap.SetOnKeysEqual ---}
procedure TGenHashMap.SetOnKeysEqual(AValue: TKeysEqual);
begin
  if AValue = nil then
    fOnKeysEqual := @DefaultKeysEqual
  else
    fOnKeysEqual := AValue;
end;

{--- TGenHashMap.SetOnKeyToString ---}
procedure TGenHashMap.SetOnKeyToString(AValue: TKeyToString);
begin
  if AValue = nil then
    fOnKeyToString := @DefaultKeyToString
  else
    fOnKeyToString:=AValue;
end;

{--- TGenHashMap.CursorIsFirst ---}
function TGenHashMap.CursorIsFirst(const Cursor: THashMapCursor): Boolean;
var
  Map : TGenHashMap;
begin
  Map := Cursor.HashMap as TGenHashMap;
  Result := false;
  if Cursor.Bucket = Map.fFirstNonEmptyBucket then
    Result := Map.fBuckets[Map.fFirstNonEmptyBucket] = Cursor.Entry;
end;

{--- TGenHashMap.CursorIsLast ---}
function TGenHashMap.CursorIsLast(const Cursor: THashMapCursor): Boolean;
var
  Map : TGenHashMap;
  Entry : PEntry;
begin
  Map := Cursor.HashMap as TGenHashMap;
  Entry := PEntry(Cursor.Entry);
  Result := (Cursor.Bucket = Map.fLastNonEmptyBucket) and (Entry^.Next = nil);
end;

{--- TGenHashMap.CursorMoveNext ---}
procedure TGenHashMap.CursorMoveNext(const Cursor: THashMapCursor);
var
  Map : TGenHashMap;
begin
  if Cursor.Bucket <> -1 then
  begin
    Map := Cursor.HashMap as TGenHashMap;

    Cursor.Previous := Cursor.Entry;
    Cursor.Entry := PEntry(Cursor.Entry)^.Next;
    if Cursor.Entry = nil then
    begin
      Cursor.Bucket := Map.NextNonEmptyBucket(Cursor.Bucket + 1);
      Cursor.Previous := nil;
      if Cursor.Bucket >= 0 then
        Cursor.Entry := Map.fBuckets[Cursor.Bucket];
    end;
  end;
end;

{--- TGenHashMap.SetItemAt ---}
procedure TGenHashMap.SetItemAt(const Position: THashMapCursor; AValue: _TItem_);
begin
  GetEntryAt(Position)^.Value := AValue;
end;

{--- TGenHashMap.ToString ---}
function TGenHashMap.ToString: String;
var
  Bucket, LastBucket, I : Integer;
  Entry : PEntry;
begin
  Result := '{';

  I := 1;
  LastBucket := fBucketCount - 1;
  for Bucket := 0 to LastBucket do
  begin
    Entry := fBuckets[Bucket];

    while Entry <> nil do
    begin
      Result := Result + '(' + fOnKeyToString(Entry^.Key) + '=>' +
        fOnItemToString(Entry^.Value) + ')';

      if I < fSize then
        Result := Result + ', ';

      Inc(I);
      Entry := Entry^.Next;
    end;
  end;

  Result := Result + '}';
end;

{======================}
{=== THashSetCursor ===}
{======================}

{--- THashSetCursor.Equals ---}
function THashSetCursor.Equals(const Cursor: THashSetCursor): Boolean;
begin
  Result := fPos.Equals(Cursor.fPos)
end;

{--- THashSetCursor.HasItem ---}
function THashSetCursor.HasItem: Boolean;
begin
  Result := fPos.HasItem;
end;

{--- THashSetCursor.Init ---}
constructor THashSetCursor.Init(HashSet: TAbstractHashSet;
  const APos: THashMapCursor);
begin
  fHashSet := HashSet;
  fPos := APos;
end;

{--- THashSetCursor.IsFirst ---}
function THashSetCursor.IsFirst: Boolean;
begin
  Result := fPos.IsFirst;
end;

{--- THashSetCursor.IsLast ---}
function THashSetCursor.IsLast: Boolean;
begin
  Result := fPos.IsLast;
end;

{--- THashSetCursor.IsNil ---}
function THashSetCursor.IsNil: Boolean;
begin
  Result := fPos.IsNil;
end;

{--- THashSetCursor.MoveNext ---}
procedure THashSetCursor.MoveNext;
begin
  fPos.MoveNext;
end;

{===================}
{=== TGenHashSet ===}
{===================}

{--- TGenHashSet.Clear ---}
procedure TGenHashSet.Clear;
begin
  fMap.Clear;
end;

{--- TGenHashSet.Contains ---}
function TGenHashSet.Contains(const Item: _TItem_): Boolean;
begin
  Result := fMap.Contains(Item);
end;

{--- TGenHashSet.Create ---}
constructor TGenHashSet.Create(InitialCapacity: Integer);
begin
  Create(InitialCapacity, 0.75);
end;

{--- TGenHashSet.Create ---}
constructor TGenHashSet.Create(InitialCapacity: Integer; LoadFact: Real);
begin
  fMap := TMap.Create(InitialCapacity, LoadFact);
  fNilCursor.Init(Self, fMap.NilCursor);
  SetOnHashItem(nil);
  SetOnItemToString(nil);
  SetOnItemsEqual(nil);
end;

{--- TGenHashSet.Create ---}
constructor TGenHashSet.Create(LoadFact: Real);
begin
  Create(16, LoadFact);
end;

{--- TGenHashSet.Delete ---}
procedure TGenHashSet.Delete(const Item: _TItem_);
var
  C : THashMapCursor;
begin
  C := fMap.Find(Item);

  if C.IsNil then
    RaiseItemNotInSet;

  fMap.DeleteAt(C);
end;

{--- TGenHashSet.DeleteAt ---}
procedure TGenHashSet.DeleteAt(const Position: THashSetCursor);
begin
  fMap.DeleteAt(Position.Pos);
end;

{--- TGenHashSet.Destroy ---}
destructor TGenHashSet.Destroy;
begin
  fMap.Free;
  inherited Destroy;
end;

{--- TGenHashSet.Difference ---}
procedure TGenHashSet.Difference(Left, Right: TGenHashSet);
begin
  if Left <> Self then
  begin
    Clear;
    IncludeAll(Left);
  end;

  if Left <> Right then
    ExcludeAll(Right)
  else
    Clear;
end;

{--- TGenHashSet.DefaultItemsEqual ---}
function TGenHashSet.DefaultItemsEqual(const A, B: _TItem_): Boolean;
begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := true;
end;

{--- TGenHashSet.DefaultItemToString ---}
function TGenHashSet.DefaultItemToString(const Item: _TItem_): String;
begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
end;

{--- TGenHashSet.DefaultHashItem ---}
function TGenHashSet.DefaultHashItem(const Item: _TItem_): Integer;
begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := 0;
end;

{--- TGenHashSet.EnumeratorGet ---}
function TGenHashSet.EnumeratorGet(const Pos: THashSetCursor): _TItem_;
begin
  ReadItemAt(Pos, Result);
end;

{--- TGenHashSet.EnumeratorNext ---}
function TGenHashSet.EnumeratorNext(var Pos: THashSetCursor): Boolean;
begin
  if Pos.IsNil then
    Pos := First
  else
    Pos.MoveNext;
  Result := Pos.HasItem;
end;

{--- TGenHashSet.ExchangeContent ---}
procedure TGenHashSet.ExchangeContent(ASet: TGenHashSet);
var
  Tmp : TMap;
begin
  Tmp := fMap;
  fMap := ASet.fMap;
  ASet.fMap := Tmp;
end;

{--- TGenHashSet.GetItemToString ---}
function TGenHashSet.GetItemToString: TItemToString;
begin
  Result := fMap.OnKeyToString;
end;

{--- TGenHashSet.GetOnHashItem ---}
function TGenHashSet.GetOnHashItem: THashItem;
begin
  Result := fMap.OnHashKey;
end;

{--- TGenHashSet.GetOnItemsEqual ---}
function TGenHashSet.GetOnItemsEqual: TItemEquals;
begin
  Result := fMap.OnKeysEqual;
end;

{--- TGenHashSet.Exclude ---}
procedure TGenHashSet.Exclude(const Item: _TItem_);
begin
  fMap.Exclude(Item);
end;

{--- TGenHashSet.ExcludeAll ---}
procedure TGenHashSet.ExcludeAll(ASet: TGenHashSet);
var
  C: THashMapCursor;
  I: Integer;
begin
  if ASet.GetSize > 0 then
  begin
    C := ASet.fMap.First;
    for I := 1 to ASet.GetSize do
    begin
      Exclude(ASet.fMap.Keys[C]);
      C.MoveNext;
    end;
  end;
end;

{--- TGenHashSet.First ---}
function TGenHashSet.First: THashSetCursor;
begin
  Result.Init(Self, fMap.First);
end;

{--- TGenHashSet.GetEnumerator ---}
function TGenHashSet.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(fNilCursor, @EnumeratorNext, @EnumeratorGet);
end;

{--- TGenHashSet.GetItemAt ---}
function TGenHashSet.GetItemAt(const Position: THashSetCursor): _TItem_;
begin
  fMap.ReadKeyAt(Position.Pos, Result);
end;

{--- TGenHashSet.GetSize ---}
function TGenHashSet.GetSize: Integer;
begin
  Result := fMap.Size;
end;

{--- TGenHashSet.SetOnHashItem ---}
procedure TGenHashSet.SetOnHashItem(AValue: THashItem);
begin
  if AValue = nil then
    fMap.OnHashKey := @DefaultHashItem
  else
    fMap.OnHashKey := AValue;
end;

{--- TGenHashSet.SetOnItemsEqual ---}
procedure TGenHashSet.SetOnItemsEqual(AValue: TItemEquals);
begin
  if AValue = nil then
    fMap.OnKeysEqual := @DefaultItemsEqual
  else
    fMap.OnKeysEqual := AValue;
end;

{--- TGenHashSet.SetOnItemToString ---}
procedure TGenHashSet.SetOnItemToString(AValue: TItemToString);
begin
  if AValue = nil then
    fMap.OnKeyToString := @DefaultItemToString
  else
    fMap.OnKeyToString := AValue;
end;

{--- TGenHashSet.Include ---}
procedure TGenHashSet.Include(const Item: _TItem_);
begin
  fMap.Include(Item, 0);
end;

{--- TGenHashSet.IncludeAll ---}
procedure TGenHashSet.IncludeAll(ASet: TGenHashSet);
var
  C: THashMapCursor;
  I: Integer;
begin
  if ASet.GetSize > 0 then
  begin
    C := ASet.fMap.First;
    for I := 1 to ASet.GetSize do
    begin
      Include(ASet.fMap.Keys[C]);
      C.MoveNext;
    end;
  end;
end;

{--- TGenHashSet.Insert ---}
procedure TGenHashSet.Insert(const Item: _TItem_);
var
  Inserted : Boolean;
begin
  Insert(Item, Inserted);
  if not Inserted then
    RaiseItemAlreadyInSet;
end;

{--- TGenHashSet.Insert ---}
procedure TGenHashSet.Insert(const Item: _TItem_; out Inserted: Boolean);
begin
  fMap.Insert(Item, 0, Inserted);
end;

{--- TGenHashSet.Intersection ---}
procedure TGenHashSet.Intersection(Left, Right: TGenHashSet);
var
  Inter, Tmp : TGenHashSet;
  I : Integer;
  C : THashMapCursor;
  Item : _TItem_;
begin
  if (Left.GetSize = 0) or (Right.GetSize = 0) then
    Clear
  else
  begin
    Inter := TGenHashSet.Create;
    Inter.OnHashItem := OnHashItem;
    Inter.OnItemsEqual := OnItemsEqual;
    Inter.OnItemToString := OnItemToString;

    try
      if Left.GetSize < Right.GetSize then
      begin
        Tmp := Left;
        Left := Right;
        Right := Tmp;
      end;

      C := Left.fMap.First;
      for I := 1 to Left.GetSize do
      begin
        Item := Left.fMap.Keys[C];
        if Right.fMap.Contains(Item) then
          Inter.Include(Item);
        C.MoveNext;
      end;

      ExchangeContent(Inter);
    finally
      Inter.Free;
    end;
  end;
end;

{--- TGenHashSet.IsEmpty ---}
function TGenHashSet.IsEmpty: Boolean;
begin
  Result := (fMap.Size = 0);
end;

{--- TGenHashSet.IsSubset ---}
function TGenHashSet.IsSubset(OfSet: TGenHashSet): Boolean;
var
  I : Integer;
  C : THashMapCursor;
begin
  if GetSize > 0 then
  begin
    C := fMap.First;
    for I := 1 to GetSize do
    begin
      if not OfSet.fMap.Contains(fMap.Keys[C]) then
      begin
        Result := false;
        Exit;
      end;
      C.MoveNext;
    end;
  end;
  Result := true;
end;

{--- TGenHashSet.Overlaps ---}
function TGenHashSet.Overlaps(ASet: TGenHashSet): Boolean;
var
  I : Integer;
  C : THashMapCursor;
begin
  Result := false;
  if GetSize > 0 then
  begin
    C := fMap.First;
    for I := 1 to GetSize do
    begin
      if ASet.fMap.Contains(fMap.Keys[C]) then
      begin
        Result := true;
        Break;
      end;
      C.MoveNext;
    end;
  end;
end;

{--- TGenHashSet.ReadItemAt ---}
procedure TGenHashSet.ReadItemAt(const Position: THashSetCursor;
  out Value: _TItem_);
begin
  fMap.ReadKeyAt(Position.Pos, Value);
end;

{--- TGenHashSet.SymmetricDifference ---}
procedure TGenHashSet.SymmetricDifference(Left, Right: TGenHashSet);
var
  Inter: TGenHashSet;
begin
  Inter := TGenHashSet.Create;
  Inter.OnHashItem := OnHashItem;
  Inter.OnItemsEqual := OnItemsEqual;
  Inter.OnItemToString := OnItemToString;

  try
    Inter.Intersection(Left, Right);

    Union(Left, Right);
    Difference(Self, Inter);
  finally
    Inter.Free;
  end;
end;

{--- TGenHashSet.ToString ---}
function TGenHashSet.ToString: String;
var
  C : THashMapCursor;
begin
  Result := '{';
  if GetSize > 0 then
  begin
    C := fMap.First;
    while C.HasItem do
    begin
      Result := Result + fMap.OnKeyToString(fMap.Keys[C]);
      if not C.IsLast then
        Result := Result + '; ';
      C.MoveNext;
    end;
  end;
  Result := Result + '}';
end;

{--- TGenHashSet.Union ---}
procedure TGenHashSet.Union(Left, Right: TGenHashSet);
begin
  if Left <> Self then
  begin
    Clear;
    IncludeAll(Left);
  end;

  if Left <> Right then
    IncludeAll(Right);
end;

{======================}
{=== TTreeMapCursor ===}
{======================}

{--- TTreeMapCursor.Equals ---}
function TTreeMapCursor.Equals(const Cursor: TTreeMapCursor): Boolean;
begin
  Result := (fTreeMap = Cursor.fTreeMap) and (fEntry = Cursor.fEntry);
end;

{--- TTreeMapCursor.HasItem ---}
function TTreeMapCursor.HasItem: Boolean;
begin
  Result := (fEntry <> nil);
end;

{--- TTreeMapCursor.Init ---}
constructor TTreeMapCursor.Init(Map: TAbstractTreeMap; AnEntry: Pointer);
begin
  fTreeMap := Map;
  fEntry := AnEntry;
end;

{--- TTreeMapCursor.IsFirst ---}
function TTreeMapCursor.IsFirst: Boolean;
begin
  Result := fTreeMap.CursorIsFirst(Self);
end;

{--- TTreeMapCursor.IsLast ---}
function TTreeMapCursor.IsLast: Boolean;
begin
  Result := fTreeMap.CursorIsLast(Self);
end;

{--- TTreeMapCursor.IsNil ---}
function TTreeMapCursor.IsNil: Boolean;
begin
  Result := (fEntry = nil);
end;

{--- TTreeMapCursor.MoveNext ---}
procedure TTreeMapCursor.MoveNext;
begin
  fTreeMap.CursorMoveNext(Self);
end;

{--- TTreeMapCursor.MovePrevious ---}
procedure TTreeMapCursor.MovePrevious;
begin
  fTreeMap.CursorMovePrev(Self);
end;

{===================}
{=== TGenTreeMap ===}
{===================}

{--- TGenTreeMap.Ceiling ---}
function TGenTreeMap.Ceiling(const Key: _TKey_): TTreeMapCursor;
begin
  Result.Init(Self, GetCeilingEntry(Key));
end;

{--- TGenTreeMap.Clear ---}
procedure TGenTreeMap.Clear;
begin
  DeleteTree(fRoot);
  fRoot := nil;
end;

{--- TGenTreeMap.ColorOf ---}
function TGenTreeMap.ColorOf(E: PEntry): TColor;
begin
  if E = nil then
    Result := cBlack
  else
    Result := E^.Color;
end;

{--- TGenTreeMap.Contains ---}
function TGenTreeMap.Contains(const Key: _TKey_): Boolean;
begin
  Result := GetEntry(Key) <> nil;
end;

{--- TGenTreeMap.Create ---}
constructor TGenTreeMap.Create;
begin
  inherited Create;
  fSize := 0;
  fRoot := nil;
  fNilCursor.Init(Self, nil);
  SetOnCompareKeys(nil);
  SetOnItemToString(nil);
  SetOnKeyToString(nil);
end;

{--- TGenTreeMap.DefaultCompareKeys ---}
function TGenTreeMap.DefaultCompareKeys(const A, B: _TKey_): Integer;
begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := 0;
end;

{--- TGenTreeMap.DefaultItemToString ---}
function TGenTreeMap.DefaultItemToString(const Item: _TItem_): String;
begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
end;

{--- TGenTreeMap.DefaultKeyToString ---}
function TGenTreeMap.DefaultKeyToString(const Key: _TKey_): String;
begin
  Unused(@Key);
  RaiseMethodNotRedefined;
  Result := '';
end;

{--- TGenTreeMap.Delete ---}
procedure TGenTreeMap.Delete(const Key: _TKey_);
var
  Entry : PEntry;
begin
  Entry := GetEntry(Key);
  if Entry = nil then
    RaiseKeyNotInMap;

  DeleteEntry(Entry);
end;

{--- TGenTreeMap.DeleteAt ---}
procedure TGenTreeMap.DeleteAt(const Position: TTreeMapCursor);
begin
  if Position.TreeMap <> Self then
    RaiseCursorDenotesWrongContainer;

  if Position.IsNil then
    RaiseCursorIsNil;

  DeleteEntry(Position.Entry);
end;

{--- TGenTreeMap.DeleteEntry ---}
procedure TGenTreeMap.DeleteEntry(E: PEntry);
var
  S, Replacement : PEntry;
begin
  Dec(fSize);

  if (E^.Left <> nil) and (E^.Right <> nil) then
  begin
    S := Successor(E);
    E^.Key := S^.Key;
    E^.Value := S^.Value;
    E := S;
  end;

  if E^.Left <> nil then
    Replacement := E^.Left
  else
    Replacement := E^.Right;

  if Replacement <> nil then
  begin
    Replacement^.Parent := E^.Parent;

    if E^.Parent = nil then
      fRoot := Replacement
    else if E = E^.Parent^.Left then
      E^.Parent^.Left := Replacement
    else
      E^.Parent^.Right := Replacement;

    E^.Left := nil;
    E^.Right := nil;
    E^.Parent := nil;

    if E^.Color = cBlack then
      RepairAfterDelete(Replacement);
  end
  else if E^.Parent = nil then
    fRoot := nil
  else
  begin
    if E^.Color = cBlack then
      RepairAfterDelete(E);

    if E^.Parent <> nil then
    begin
      if E = E^.Parent^.Left then
        E^.Parent^.Left := nil
      else if E = E^.Parent^.Right then
        E^.Parent^.Right := nil;

      E^.Parent := nil;
    end;
  end;
  Dispose(E);
end;

{--- TGenTreeMap.DeleteFirst ---}
procedure TGenTreeMap.DeleteFirst;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  DeleteEntry(GetFirstEntry);
end;

{--- TGenTreeMap.DeleteLast ---}
procedure TGenTreeMap.DeleteLast;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  DeleteEntry(GetLastEntry);
end;

{--- TGenTreeMap.DeleteTree ---}
procedure TGenTreeMap.DeleteTree(E: PEntry);
var
  R, L : PEntry;
begin
  while true do
  begin
    if E = nil then
      Exit;

    R := E^.Right;
    L := E^.Left;

    Dispose(E);
    Dec(fSize);

    DeleteTree(L);

    E := R;
  end;
end;

{--- TGenTreeMap.EnumeratorGet ---}
function TGenTreeMap.EnumeratorGet(const Pos: TTreeMapCursor): _TItem_;
begin
  ReadItemAt(Pos, Result);
end;

{--- TGenTreeMap.EnumeratorNext ---}
function TGenTreeMap.EnumeratorNext(var Pos: TTreeMapCursor): Boolean;
begin
  if Pos.IsNil then
    Pos := First
  else
    Pos.MoveNext;
  Result := Pos.HasItem;
end;

{--- TGenTreeMap.Destroy ---}
destructor TGenTreeMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{--- TGenTreeMap.Exclude ---}
procedure TGenTreeMap.Exclude(const Key: _TKey_);
var
  Entry: PEntry;
begin
  Entry := GetEntry(Key);
  if Entry <> nil then
    DeleteEntry(Entry);
end;

{--- TGenTreeMap.Find ---}
function TGenTreeMap.Find(const Key: _TKey_): TTreeMapCursor;
begin
  Result.Init(Self, GetEntry(Key));
end;

{--- TGenTreeMap.First ---}
function TGenTreeMap.First: TTreeMapCursor;
begin
  Result.Init(Self, GetFirstEntry);
end;

{--- TGenTreeMap.FirstItem ---}
function TGenTreeMap.FirstItem: _TItem_;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := GetFirstEntry^.Value;
end;

{--- TGenTreeMap.FirstKey ---}
function TGenTreeMap.FirstKey: _TKey_;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := GetFirstEntry^.Key;
end;

{--- TGenTreeMap.RepairAfterDelete ---}
procedure TGenTreeMap.RepairAfterDelete(E: PEntry);
var
  Sib : PEntry;
begin
  while (E <> fRoot) and (ColorOf(E) = cBlack) do
  begin
    if E = LeftOf(ParentOf(E)) then
    begin
      Sib := RightOf(ParentOf(E));

      if ColorOf(Sib) = cRed then
      begin
        SetColor(Sib, cBlack);
        SetColor(ParentOf(E), cRed);
        RotateLeft(ParentOf(E));
        Sib := RightOf(ParentOf(E));
      end;

      if (ColorOf(LeftOf(Sib)) = cBlack) and (ColorOf(RightOf(Sib)) = cBlack) then
      begin
        SetColor(Sib, cRed);
        E := ParentOf(E);
      end
      else
      begin
        if ColorOf(RightOf(Sib)) = cBlack then
        begin
          SetColor(LeftOf(Sib), cBlack);
          SetColor(Sib, cRed);
          RotateRight(Sib);
          Sib := RightOf(ParentOf(E));
        end;

        SetColor(Sib, ColorOf(ParentOf(E)));
        SetColor(ParentOf(E), cBlack);
        SetColor(RightOf(Sib), cBlack);
        RotateLeft(ParentOf(E));
        E := fRoot;
      end;
    end
    else
    begin
      Sib := LeftOf(ParentOf(E));

      if ColorOf(Sib) = cRed then
      begin
        SetColor(Sib, cBlack);
        SetColor(ParentOf(E), cRed);
        RotateRight(ParentOf(E));
        Sib := LeftOf(ParentOf(E));
      end;

      if (ColorOf(RightOf(Sib)) = cBlack) and (ColorOf(LeftOf(Sib)) = cBlack) then
      begin
        SetColor(Sib, cRed);
        E := ParentOf(E);
      end
      else
      begin

        if ColorOf(LeftOf(Sib)) = cBlack then
        begin
          SetColor(RightOf(Sib), cBlack);
          SetColor(Sib, cRed);
          RotateLeft(Sib);
          Sib := LeftOf(ParentOf(E));
        end;

        SetColor(Sib, ColorOf(ParentOf(E)));
        SetColor(ParentOf(E), cBlack);
        SetColor(LeftOf(Sib), cBlack);
        RotateRight(ParentOf(E));
        E := fRoot;
      end;
    end;
  end;

  SetColor(E, cBlack);
end;

{--- TGenTreeMap.RepairAfterInsert ---}
procedure TGenTreeMap.RepairAfterInsert(E: PEntry);
var
  Y : PEntry;
begin
  E^.Color := cRed;

  while (E <> nil) and (E <> fRoot) and (E^.Parent^.Color = cRed) do
  begin
    if ParentOf(E) = LeftOf(ParentOf(ParentOf(E))) then
    begin
      Y := RightOf(ParentOf(ParentOf(E)));
      if ColorOf(Y) = cRed then
      begin
        SetColor(ParentOf(E), cBlack);
        SetColor(Y, cBlack);
        SetColor(ParentOf(ParentOf(E)), cRed);
        E := ParentOf(ParentOf(E));
      end
      else
      begin
        if E = RightOf(ParentOf(E)) then
        begin
          E := ParentOf(E);
          RotateLeft(E);
        end;
        SetColor(ParentOf(E), cBlack);
        SetColor(ParentOf(ParentOf(E)), cRed);
        RotateRight(ParentOf(ParentOf(E)));
      end;
    end
    else
    begin
      Y := LeftOf(ParentOf(ParentOf(E)));
      if ColorOf(Y) = cRed then
      begin
        SetColor(ParentOf(E), cBlack);
        SetColor(Y, cBlack);
        SetColor(ParentOf(ParentOf(E)), cRed);
        E := ParentOf(ParentOf(E));
      end
      else
      begin
        if E = LeftOf(ParentOf(E)) then
        begin
          E := ParentOf(E);
          RotateRight(E);
        end;
        SetColor(ParentOf(E), cBlack);
        SetColor(ParentOf(ParentOf(E)), cRed);
        RotateLeft(ParentOf(ParentOf(E)));
      end;
    end;
  end;

  fRoot^.Color := cBlack;
end;

{--- TGenTreeMap.Floor ---}
function TGenTreeMap.Floor(const Key: _TKey_): TTreeMapCursor;
begin
  Result.Init(Self, GetFloorEntry(Key));
end;

{--- TGenTreeMap.GetEnumerator ---}
function TGenTreeMap.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(fNilCursor, @EnumeratorNext, @EnumeratorGet);
end;

{--- TGenTreeMap.GetCeilingEntry ---}
function TGenTreeMap.GetCeilingEntry(const Key: _TKey_): PEntry;
var
  Cmp : Integer;
  Ch, Parent : PEntry;
begin
  Result := fRoot;
  while Result <> nil do
  begin
    Cmp := fOnCompareKeys(Key, Result^.Key);
    if Cmp < 0 then
    begin
      if Result^.Left <> nil then
        Result := Result^.Left
      else
        Exit;
    end
    else if Cmp > 0 then
    begin
      if Result^.Right <> nil then
        Result := Result^.Right
      else
      begin
        Parent := Result^.Parent;
        Ch := Result;
        while (Parent <> nil) and (Ch = Parent^.Right) do
        begin
          Ch := Parent;
          Parent := Parent^.Parent;
        end;
        Result := Parent;
        Exit;
      end;
    end
    else
      Exit;
  end;
  Result := nil;
end;

{--- TGenTreeMap.GetEntry ---}
function TGenTreeMap.GetEntry(const Key: _TKey_): PEntry;
var
  Entry: PEntry;
  Cmp : Integer;
begin
  Entry := fRoot;
  while Entry <> nil do
  begin
    Cmp := fOnCompareKeys(Key, Entry^.Key);

    if Cmp < 0 then
      Entry := Entry^.Left
    else if Cmp > 0 then
      Entry := Entry^.Right
    else
    begin
      Result := Entry;
      Exit;
    end;
  end;
  Result := nil;
end;

{--- TGenTreeMap.GetFirstEntry ---}
function TGenTreeMap.GetFirstEntry: PEntry;
begin
  Result := fRoot;
  if Result <> nil then
    while Result^.Left <> nil do
      Result := Result^.Left;
end;

{--- TGenTreeMap.GetFloorEntry ---}
function TGenTreeMap.GetFloorEntry(const Key: _TKey_): PEntry;
var
  Cmp : Integer;
  Ch, Parent : PEntry;
begin
  Result := fRoot;
  while Result <> nil do
  begin
    Cmp := fOnCompareKeys(Key, Result^.Key);
    if Cmp > 0 then
    begin
      if Result^.Right <> nil then
        Result := Result^.Right
      else
        Exit;
    end
    else if Cmp < 0 then
    begin
      if Result^.Left <> nil then
        Result := Result^.Left
      else
      begin
        Parent := Result^.Parent;
        Ch := Result;
        while (Parent <> nil) and (Ch = Parent^.Left) do
        begin
          Ch := Parent;
          Parent := Parent^.Parent;
        end;
        Result := Parent;
        Exit;
      end;
    end
    else
      Exit;
  end;
  Result := nil;
end;

{--- TGenTreeMap.GetItem ---}
function TGenTreeMap.GetItem(const Key: _TKey_): _TItem_;
var
  Entry : PEntry;
begin
  Entry := GetEntry(Key);
  if Entry = nil then
    RaiseKeyNotInMap;

  Result := Entry^.Value;
end;

{--- TGenTreeMap.GetItemAt ---}
function TGenTreeMap.GetItemAt(const Position: TTreeMapCursor): _TItem_;
begin
  if Position.TreeMap <> Self then
    RaiseCursorDenotesWrongContainer;

  if Position.IsNil then
    RaiseCursorIsNil;

  Result := PEntry(Position.Entry)^.Value;
end;

{--- TGenTreeMap.GetKeyAt ---}
function TGenTreeMap.GetKeyAt(const Position: TTreeMapCursor): _TKey_;
begin
  if Position.TreeMap <> Self then
    RaiseCursorDenotesWrongContainer;

  if Position.IsNil then
    RaiseCursorIsNil;

  Result := PEntry(Position.Entry)^.Key;
end;

{--- TGenTreeMap.GetLastEntry ---}
function TGenTreeMap.GetLastEntry: PEntry;
begin
  Result := fRoot;
  if Result <> nil then
    while Result^.Right <> nil do
      Result := Result^.Right;
end;

{--- TGenTreeMap.Include ---}
procedure TGenTreeMap.Include(const Key: _TKey_; const Value: _TItem_);
var
  T, Parent, N : PEntry;
  Cmp : Integer;
begin
  if fRoot = nil then
  begin
    fRoot := NewEntry(nil, Key, Value);
    fSize := 1;
  end
  else
  begin
    T := fRoot;
    repeat
      Parent := T;
      Cmp := fOnCompareKeys(Key, T^.Key);
      if Cmp < 0 then
        T := T^.Left
      else if Cmp > 0 then
        T := T^.Right
      else
      begin
        T^.Value := Value;
        Exit;
      end;
    until T = nil;

    N := NewEntry(Parent, Key, Value);
    if Cmp < 0 then
      Parent^.Left := N
    else
      Parent^.Right := N;
    RepairAfterInsert(N);
    Inc(fSize);
  end;
end;

{--- TGenTreeMap.Insert ---}
procedure TGenTreeMap.Insert(const Key: _TKey_; const Value: _TItem_);
var
  Inserted : Boolean;
begin
  Insert(Key, Value, Inserted);
  if not Inserted then
    RaiseKeyAlreadyInMap;
end;

{--- TGenTreeMap.Insert ---}
procedure TGenTreeMap.Insert(const Key: _TKey_; const Value: _TItem_; out
  Inserted: Boolean);
var
  T, Parent, N : PEntry;
  Cmp : Integer;
begin
  Inserted := false;
  if fRoot = nil then
  begin
    fRoot := NewEntry(nil, Key, Value);
    fSize := 1;
    Inserted := true;
  end
  else
  begin
    T := fRoot;
    repeat
      Parent := T;
      Cmp := fOnCompareKeys(Key, T^.Key);
      if Cmp < 0 then
        T := T^.Left
      else if Cmp > 0 then
        T := T^.Right
      else
        Exit;
    until T = nil;

    N := NewEntry(Parent, Key, Value);
    if Cmp < 0 then
      Parent^.Left := N
    else
      Parent^.Right := N;
    RepairAfterInsert(N);
    Inc(fSize);
    Inserted := true;
  end;
end;

{--- TGenTreeMap.IsEmpty ---}
function TGenTreeMap.IsEmpty: Boolean;
begin
  Result := (fSize = 0);
end;

{--- TGenTreeMap.Last ---}
function TGenTreeMap.Last: TTreeMapCursor;
begin
  Result.Init(Self, GetLastEntry);
end;

{--- TGenTreeMap.LastItem ---}
function TGenTreeMap.LastItem: _TItem_;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := GetLastEntry^.Value;
end;

{--- TGenTreeMap.LastKey ---}
function TGenTreeMap.LastKey: _TKey_;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Result := GetLastEntry^.Key;
end;

{--- TGenTreeMap.LeftOf ---}
function TGenTreeMap.LeftOf(E: PEntry): PEntry;
begin
  if E = nil then
    Result := nil
  else
    Result := E^.Left;
end;

{--- TGenTreeMap.NewEntry ---}
function TGenTreeMap.NewEntry(AParent: PEntry; const AKey: _TKey_;
  const AValue: _TItem_) : PEntry;
begin
  New(Result);
  Result^.Parent := AParent;
  Result^.Key := AKey;
  Result^.Value := AValue;
  Result^.Left := nil;
  Result^.Right := nil;
end;

{--- TGenTreeMap.ParentOf ---}
function TGenTreeMap.ParentOf(E: PEntry): PEntry;
begin
  if E = nil then
    Result := nil
  else
    Result := E^.Parent;
end;

{--- TGenTreeMap.Predecessor ---}
function TGenTreeMap.Predecessor(E: PEntry): PEntry;
var
  Ch : PEntry;
begin
  if E = nil then
    Result := nil
  else if E^.Left <> nil then
  begin
    Result := E^.Left;
    while Result^.Right <> nil do
      Result := Result^.Right;
  end
  else
  begin
    Result := E^.Parent;
    Ch := E;
    while (Result <> nil) and (Ch = Result^.Left) do
    begin
      Ch := Result;
      Result := Result^.Parent;
    end;
  end;
end;

{--- TGenTreeMap.ReadFirstItem ---}
procedure TGenTreeMap.ReadFirstItem(out Value : _TItem_);
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Value := GetFirstEntry^.Value;
end;

{--- TGenTreeMap.ReadFirstKey ---}
procedure TGenTreeMap.ReadFirstKey(out Key : _TKey_); inline;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Key := GetFirstEntry^.Key;
end;

{--- TGenTreeMap.ReadItem ---}
procedure TGenTreeMap.ReadItem(const Key: _TKey_; out Value: _TItem_);
var
  Entry : PEntry;
begin
  Entry := GetEntry(Key);
  if Entry = nil then
    RaiseKeyNotInMap;

  Value := Entry^.Value;
end;

{--- TGenTreeMap.ReadItemAt ---}
procedure TGenTreeMap.ReadItemAt(const Position: TTreeMapCursor; out Value: _TItem_);
begin
  if Position.TreeMap <> Self then
    RaiseCursorDenotesWrongContainer;

  if Position.IsNil then
    RaiseCursorIsNil;

  Value :=  PEntry(Position.Entry)^.Value;
end;

{--- TGenTreeMap.ReadKeyAt ---}
procedure TGenTreeMap.ReadKeyAt(const Position : TTreeMapCursor; out Key: _TKey_);
begin
  if Position.TreeMap <> Self then
    RaiseCursorDenotesWrongContainer;

  if Position.IsNil then
    RaiseCursorIsNil;

  Key :=  PEntry(Position.Entry)^.Key;
end;

{--- TGenTreeMap.ReadLastItem ---}
procedure TGenTreeMap.ReadLastItem(out Value : _TItem_);
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Value := GetLastEntry^.Value;
end;

{--- TGenTreeMap.ReadLastKey ---}
procedure TGenTreeMap.ReadLastKey(out Key : _TKey_); inline;
begin
  if fSize = 0 then
    RaiseContainerEmpty;

  Key := GetLastEntry^.Key;
end;

{--- TGenTreeMap.Replace ---}
procedure TGenTreeMap.Replace(const Key: _TKey_; const Value: _TItem_);
var
  Entry : PEntry;
begin
  Entry := GetEntry(Key);
  if Entry = nil then
    RaiseKeyNotInMap;

  Entry^.Value := Value;
end;

{--- TGenTreeMap.RightOf ---}
function TGenTreeMap.RightOf(E: PEntry): PEntry;
begin
  if E = nil then
    Result := nil
  else
    Result := E^.Right;
end;

{--- TGenTreeMap.RotateLeft ---}
procedure TGenTreeMap.RotateLeft(E: PEntry);
var
  R : PEntry;
begin
  if E <> nil then
  begin
    R := E^.Right;

    E^.Right := R^.Left;

    if R^.Left <> nil then
      R^.Left^.Parent := E;

    R^.Parent := E^.Parent;
    if E^.Parent = nil then
      fRoot := R
    else if E^.Parent^.Left = E then
      E^.Parent^.Left := R
    else
      E^.Parent^.Right := R;
    R^.Left := E;
    E^.Parent := R;
  end;
end;

{--- TGenTreeMap.RotateRight ---}
procedure TGenTreeMap.RotateRight(E: PEntry);
var
  L : PEntry;
begin
  if E <> nil then
  begin
    L := E^.Left;
    E^.Left := L^.Right;
    if L^.Right <> nil then
      L^.Right^.Parent := E;
    L^.Parent := E^.Parent;
    if E^.Parent = nil then
      fRoot := L
    else if E^.Parent^.Right = E then
      E^.Parent^.Right := L
    else
      E^.Parent^.Left := L;
    L^.Right := E;
    E^.Parent := L;
  end;
end;

{--- TGenTreeMap.SetColor ---}
procedure TGenTreeMap.SetColor(E: PEntry; Color: TColor);
begin
  if E <> nil then
    E^.Color := Color;
end;

{--- TGenTreeMap.SetOnCompareKeys ---}
procedure TGenTreeMap.SetOnCompareKeys(AValue: TCompareKeys);
begin
  if AValue = nil then
    fOnCompareKeys := @DefaultCompareKeys
  else
    fOnCompareKeys := AValue;
end;

{--- TGenTreeMap.SetOnItemToString ---}
procedure TGenTreeMap.SetOnItemToString(AValue: TItemToString);
begin
  if AValue = nil then
    fOnItemToString := @DefaultItemToString
  else
    fOnItemToString := AValue;
end;

{--- TGenTreeMap.SetOnKeyToString ---}
procedure TGenTreeMap.SetOnKeyToString(AValue: TKeyToString);
begin
  if AValue = nil then
    fOnKeyToString := @DefaultKeyToString
  else
    fOnKeyToString := AValue;
end;

{--- TGenTreeMap.SetItemAt ---}
procedure TGenTreeMap.SetItemAt(const Position: TTreeMapCursor; Value: _TItem_);
begin
  if Position.TreeMap <> Self then
    RaiseCursorDenotesWrongContainer;

  if Position.IsNil then
    RaiseCursorIsNil;

  PEntry(Position.Entry)^.Value := Value;
end;

{--- TGenTreeMap.Successor ---}
function TGenTreeMap.Successor(E: PEntry): PEntry;
var
  P, Ch : PEntry;
begin
  if E = nil then
    Result := nil
  else if E^.Right <> nil then
  begin
    P := E^.Right;
    while P^.Left <> nil do
      P := P^.Left;
    Result := P;
  end
  else
  begin
    P := E^.Parent;
    Ch := E;
    while (P <> nil) and (Ch = P^.Right) do
    begin
      Ch := P;
      P := P^.Parent;
    end;
    Result := P;
  end;
end;

{--- TGenTreeMap.CursorIsFirst ---}
function TGenTreeMap.CursorIsFirst(const Cursor: TTreeMapCursor): Boolean;
begin
  Result := (Cursor.Entry <> nil)
    and (Cursor.Entry = (Cursor.TreeMap as TGenTreeMap).GetFirstEntry);
end;

{--- TGenTreeMap.CursorIsLast ---}
function TGenTreeMap.CursorIsLast(const Cursor: TTreeMapCursor): Boolean;
begin
  Result := (Cursor.Entry <> nil)
    and (Cursor.Entry = (Cursor.TreeMap as TGenTreeMap).GetLastEntry);
end;

{--- TGenTreeMap.CursorMoveNext ---}
procedure TGenTreeMap.CursorMoveNext(const Cursor: TTreeMapCursor);
begin
  if Cursor.Entry <> nil then
    Cursor.Entry := (Cursor.TreeMap as TGenTreeMap).Successor(Cursor.Entry);
end;

{--- TGenTreeMap.CursorMovePrev ---}
procedure TGenTreeMap.CursorMovePrev(const Cursor: TTreeMapCursor);
begin
  if Cursor.Entry <> nil then
    Cursor.Entry := (Cursor.TreeMap as TGenTreeMap).Predecessor(Cursor.Entry);
end;

{--- TGenTreeMap.ToString ---}
function TGenTreeMap.ToString: String;
var
  Entry, LastEntry : PEntry;
begin
  Result := '{';

  LastEntry := GetLastEntry;

  Entry := GetFirstEntry;
  while Entry <> nil do
  begin
    Result := Result + '(' + fOnKeyToString(Entry^.Key) + '=>' +
      fOnItemToString(Entry^.Value) + ')';

    if Entry <> LastEntry then
      Result := Result + ', ';

    Entry := Successor(Entry);
  end;

  Result := Result + '}';
end;

{======================}
{=== TTreeSetCursor ===}
{======================}

{--- TTreeSetCursor.Equals ---}
function TTreeSetCursor.Equals(const Cursor: TTreeSetCursor): Boolean;
begin
  Result := fPos.Equals(Cursor.fPos)
end;

{--- TTreeSetCursor.HasItem ---}
function TTreeSetCursor.HasItem: Boolean;
begin
  Result := fPos.HasItem;
end;

{--- TTreeSetCursor.Init ---}
constructor TTreeSetCursor.Init(TreeSet: TAbstractTreeSet; const APos: TTreeMapCursor);
begin
  fTreeSet := TreeSet;
  fPos := APos;
end;

{--- TTreeSetCursor.IsFirst ---}
function TTreeSetCursor.IsFirst: Boolean;
begin
  Result := fPos.IsFirst;
end;

{--- TTreeSetCursor.IsLast ---}
function TTreeSetCursor.IsLast: Boolean;
begin
  Result := fPos.IsLast;
end;

{--- TTreeSetCursor.IsNil ---}
function TTreeSetCursor.IsNil: Boolean;
begin
  Result := fPos.IsNil;
end;

{--- TTreeSetCursor.MoveNext ---}
procedure TTreeSetCursor.MoveNext;
begin
  fPos.MoveNext;
end;

{--- TTreeSetCursor.MovePrevious ---}
procedure TTreeSetCursor.MovePrevious;
begin
  fPos.MovePrevious;
end;

{===================}
{=== TGenTreeSet ===}
{===================}

{--- TGenTreeSet.Ceiling ---}
function TGenTreeSet.Ceiling(const Item: _TItem_): TTreeSetCursor;
begin
  Result.Init(Self, fMap.Ceiling(Item));
end;

{--- TGenTreeSet.Clear ---}
procedure TGenTreeSet.Clear;
begin
  fMap.Clear;
end;

{--- TGenTreeSet.Contains ---}
function TGenTreeSet.Contains(const Item: _TItem_): Boolean;
begin
  Result := fMap.Contains(Item);
end;

{--- TGenTreeSet.Create ---}
constructor TGenTreeSet.Create;
begin
  fMap := TMap.Create;
  fNilCursor.Init(Self, fMap.NilCursor);
  SetOnCompareItems(nil);
  SetOnItemToString(nil);
end;

{--- TGenTreeSet.DefaultCompareItems ---}
function TGenTreeSet.DefaultCompareItems(const A, B: _TItem_): Integer;
begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := 0;
end;

{--- TGenTreeSet.DefaultItemToString ---}
function TGenTreeSet.DefaultItemToString(const Item: _TItem_): String;
begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
end;

{--- TGenTreeSet.Delete ---}
procedure TGenTreeSet.Delete(const Item: _TItem_);
var
  C : TTreeMapCursor;
begin
  C := fMap.Find(Item);

  if C.IsNil then
    RaiseItemNotInSet;

  fMap.DeleteAt(C);
end;

{--- TGenTreeSet.DeleteAt ---}
procedure TGenTreeSet.DeleteAt(const Position: TTreeSetCursor);
begin
  fMap.DeleteAt(Position.Pos);
end;

{--- TGenTreeSet.DeleteFirst ---}
procedure TGenTreeSet.DeleteFirst;
begin
  fMap.DeleteFirst;
end;

{--- TGenTreeSet.DeleteLast ---}
procedure TGenTreeSet.DeleteLast;
begin
  fMap.DeleteLast;
end;

{--- TGenTreeSet.Destroy ---}
destructor TGenTreeSet.Destroy;
begin
  fMap.Free;
  inherited;
end;

{--- TGenTreeSet.Difference ---}
procedure TGenTreeSet.Difference(Left, Right: TGenTreeSet);
begin
  if Left <> Self then
  begin
    Clear;
    IncludeAll(Left);
  end;

  if Left <> Right then
    ExcludeAll(Right)
  else
    Clear;
end;

{--- TGenTreeSet.EnumeratorGet ---}
function TGenTreeSet.EnumeratorGet(const Pos: TTreeSetCursor): _TItem_;
begin
  ReadItemAt(Pos, Result);
end;

{--- TGenTreeSet.EnumeratorNext ---}
function TGenTreeSet.EnumeratorNext(var Pos: TTreeSetCursor): Boolean;
begin
  if Pos.IsNil then
    Pos := First
  else
    Pos.MoveNext;
  Result := Pos.HasItem;
end;

{--- TGenTreeSet.ExchangeContent ---}
procedure TGenTreeSet.ExchangeContent(ASet: TGenTreeSet);
var
  Tmp : TMap;
begin
  Tmp := fMap;
  fMap := ASet.fMap;
  ASet.fMap := Tmp;
end;

{--- TGenTreeSet.GetOnCompareItems ---}
function TGenTreeSet.GetOnCompareItems: TCompareItems;
begin
  Result := fMap.OnCompareKeys;
end;

{--- TGenTreeSet.GetOnItemToString ---}
function TGenTreeSet.GetOnItemToString: TItemToString;
begin
  Result := fMap.OnKeyToString;
end;

{--- TGenTreeSet.Exclude ---}
procedure TGenTreeSet.Exclude(const Item: _TItem_);
begin
  fMap.Exclude(Item);
end;

{--- TGenTreeSet.ExcludeAll ---}
procedure TGenTreeSet.ExcludeAll(ASet: TGenTreeSet);
var
  C: TTreeMapCursor;
  I: Integer;
begin
  if ASet.GetSize > 0 then
  begin

    C := ASet.fMap.First;
    for I := 1 to ASet.GetSize do
    begin
      Exclude(ASet.fMap.Keys[C]);
      C.MoveNext;
    end;
  end;
end;

{--- TTreeSetCursor ---}
function TGenTreeSet.First: TTreeSetCursor;
begin
  Result.Init(Self, fMap.First);
end;

{--- TGenTreeSet.FirstItem ---}
function TGenTreeSet.FirstItem: _TItem_;
begin
  fMap.ReadFirstKey(Result);
end;

{--- TGenTreeSet.Floor ---}
function TGenTreeSet.Floor(const Item: _TItem_): TTreeSetCursor;
begin
  Result.Init(Self, fMap.Floor(Item));
end;

{--- TGenTreeSet.GetEnumerator ---}
function TGenTreeSet.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(fNilCursor, @EnumeratorNext, @EnumeratorGet);
end;

{--- TGenTreeSet.GetItemAt ---}
function TGenTreeSet.GetItemAt(const Position: TTreeSetCursor): _TItem_;
begin
  fMap.ReadKeyAt(Position.Pos, Result);
end;

{--- TGenTreeSet.GetSize ---}
function TGenTreeSet.GetSize: Integer;
begin
  Result := fMap.Size;
end;

{--- TGenTreeSet.SetOnCompareItems ---}
procedure TGenTreeSet.SetOnCompareItems(AValue: TCompareItems);
begin
  if AValue = nil then
    fMap.OnCompareKeys := @DefaultCompareItems
  else
    fMap.OnCompareKeys := AValue;
end;

{--- TGenTreeSet.SetOnItemToString ---}
procedure TGenTreeSet.SetOnItemToString(AValue: TItemToString);
begin
  if AValue = nil then
    fMap.OnKeyToString := @DefaultItemToString
  else
    fMap.OnKeyToString := AValue;
end;

{--- TGenTreeSet.Include ---}
procedure TGenTreeSet.Include(const Item: _TItem_);
begin
  fMap.Include(Item, 0);
end;

{--- TGenTreeSet.IncludeAll ---}
procedure TGenTreeSet.IncludeAll(ASet: TGenTreeSet);
var
  C: TTreeMapCursor;
  I: Integer;
begin
  if ASet.GetSize > 0 then
  begin
    C := ASet.fMap.First;

    for I := 1 to ASet.GetSize do
    begin
      Include(ASet.fMap.Keys[C]);
      C.MoveNext;
    end;
  end;
end;

{--- TGenTreeSet.Insert ---}
procedure TGenTreeSet.Insert(const Item: _TItem_);
var
  Inserted : Boolean;
begin
  Insert(Item, Inserted);
  if not Inserted then
    RaiseItemAlreadyInSet;
end;

{--- TGenTreeSet.Insert ---}
procedure TGenTreeSet.Insert(const Item: _TItem_; out Inserted: Boolean);
begin
  fMap.Insert(Item, 0, Inserted);
end;

{--- TGenTreeSet.Intersection ---}
procedure TGenTreeSet.Intersection(Left, Right: TGenTreeSet);
var
  Inter, Tmp : TGenTreeSet;
  I : Integer;
  C : TTreeMapCursor;
  Item : _TItem_;
begin
  if (Left.GetSize = 0) or (Right.GetSize = 0) then
    Clear
  else
  begin
    Inter := TGenTreeSet.Create;
    Inter.OnCompareItems := OnCompareItems;
    Inter.OnItemToString := OnItemToString;

    try
      if Left.GetSize < Right.GetSize then
      begin
        Tmp := Left;
        Left := Right;
        Right := Tmp;
      end;

      C := Left.fMap.First;
      for I := 1 to Left.GetSize do
      begin
        Item := Left.fMap.Keys[C];
        if Right.fMap.Contains(Item) then
          Inter.Include(Item);
        C.MoveNext;
      end;

      ExchangeContent(Inter);
    finally
      Inter.Free;
    end;
  end;
end;

{--- TGenTreeSet.IsEmpty ---}
function TGenTreeSet.IsEmpty: Boolean;
begin
  Result := fMap.Size = 0;
end;

{--- TGenTreeSet.IsSubset ---}
function TGenTreeSet.IsSubset(OfSet: TGenTreeSet): Boolean;
var
  I : Integer;
  C : TTreeMapCursor;
begin
  if GetSize > 0 then
  begin
    C := fMap.First;
    for I := 1 to GetSize do
    begin
      if not OfSet.fMap.Contains(fMap.Keys[C]) then
      begin
        Result := false;
        Exit;
      end;
      C.MoveNext;
    end;
  end;
  Result := true;
end;

{--- TGenTreeSet.Last ---}
function TGenTreeSet.Last: TTreeSetCursor;
begin
  Result.Init(Self, fMap.Last);
end;

{--- TGenTreeSet.LastItem ---}
function TGenTreeSet.LastItem: _TItem_;
begin
  fMap.ReadLastKey(Result);
end;

{--- TGenTreeSet.Overlaps ---}
function TGenTreeSet.Overlaps(ASet: TGenTreeSet): Boolean;
var
  I : Integer;
  C : TTreeMapCursor;
begin
  Result := false;
  if GetSize > 0 then
  begin
    C := fMap.First;
    for I := 1 to GetSize do
    begin
      if ASet.fMap.Contains(fMap.Keys[C]) then
      begin
        Result := true;
        Break;
      end;
      C.MoveNext;
    end;
  end;
end;

{--- TGenTreeSet.ReadFirstItem ---}
procedure TGenTreeSet.ReadFirstItem(out Value : _TItem_);
begin
  fMap.ReadFirstKey(Value);
end;

{--- TGenTreeSet.ReadItemAt ---}
procedure TGenTreeSet.ReadItemAt(const Position: TTreeSetCursor;
  out Value: _TItem_);
begin
  fMap.ReadKeyAt(Position.Pos, Value);
end;

{--- TGenTreeSet.ReadLastItem ---}
procedure TGenTreeSet.ReadLastItem(out Value : _TItem_);
begin
  fMap.ReadLastKey(Value);
end;

{--- TGenTreeSet.SymmetricDifference ---}
procedure TGenTreeSet.SymmetricDifference(Left, Right: TGenTreeSet);
var
  Inter: TGenTreeSet;
begin
  Inter := TGenTreeSet.Create;
  Inter.OnCompareItems := OnCompareItems;
  Inter.OnItemToString := OnItemToString;
  try
    Inter.Intersection(Left, Right);
    Union(Left, Right);
    Difference(Self, Inter);
  finally
    Inter.Free;
  end;
end;

{--- TGenTreeSet.ToString ---}
function TGenTreeSet.ToString: String;
var
  C : TTreeMapCursor;
begin
  Result := '{';

  if GetSize > 0 then
  begin
    C := fMap.First;
    while C.HasItem do
    begin
      Result := Result + fMap.OnKeyToString(fMap.Keys[C]);
      if not C.IsLast then
        Result := Result + '; ';
      C.MoveNext;
    end;
  end;

  Result := Result + '}';
end;

{--- TGenTreeSet.Union ---}
procedure TGenTreeSet.Union(Left, Right: TGenTreeSet);
begin
  if Left <> Self then
  begin
    Clear;
    IncludeAll(Left);
  end;

  if Left <> Right then
    IncludeAll(Right);
end;

end.
