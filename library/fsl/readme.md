# Object life-cycle management

The pascal library uses it's own approach to managing object life 
cycles: manual reference counting.

Each object keeps it's own internal reference count,
and when a new pointer to the object is created, the programmer
must increment the count. When all references to the object 
are released, the object is automatically freed.

Question: Why not use interfaces?
Answers: 
* well, the outcome is kind of like how interfaces work, but without their polymorphism problems (can't cast an interface to a descendent interface)
* legacy, really. This approach was implemented prior to interfaces being available

The way it works is all implemented in TFslObject

Using an object is easy: 

    var
      obj : TFslObject; { or any descendent }
    begin
      obj := TFslObject.create;
      try
        // do things with the object
      finally
        obj.free;
      end;
    end;

Note: you *never* call FreeAndNil(obj) - because FreeAndNil
calls TObject(obj).free, and it can't be overridden. So 
you can't ever call FreeAndNil when working with these objects.
It's painful, but if you look at the existing code, 
you just don't ever need to do it very often - use smaller
procedures, mostly.  (or use the := nil trick below)

The technique starts to get useful when you have to add the object to
a list:

    Procedure addToList(list : TFslObjectList; params...);
    var
      obj : TFslObject; { or any descendent }
    begin
      obj := TFslObject.create;
      try
        // do things with the object and params
        list.add(obj.link);  // add to the reference count
      finally
        obj.free; // delete from the reference count, and free if no more references are left
      end;
    end;

This code won't leak - if the object can't be created, an exception
will be raised, and the object will be released. If it works, it'll be 
added to the list and the list will be the only owner - the .link
routine adds to the reference count. 

So once the object is in the list, and you want to work with it:

    procedure UseObject(list : TFslObjectList; i : integer);
    var
      obj : TFslObject;
    begin
      obj := list[i].link; // get a copy, and reference count it
      try
        // do stuff with the object. It's ok to keep 
        // doing stuff, even if it is removed from the list
        // while this is happening 
      finally
        obj.free; // release our reference to obj; free if this is the last reference
      end; 
    end;

The list automatically owns the object, and manages it's own reference to it
correctly. You don't have to increment the reference count when you use an
object:

    procedure UseObject(list : TFslObjectList; i : integer);
    var
      obj : TFslObject;
    begin
      obj := list[i]; 
      // do stuff with the object, but be quick:
      // if it gets removed from the list while you're using 
      // it (e.g. in a routine you call), then boom!
    end;


If you have properties that have this type, then you code them like this:

    Property Object_ : TFslObject read FObject write SetObject;
  
and SetObject looks like this:

    Procedure TSomeObject.SetObject(value : TFslObject);
    begin
      FObject.free; // release the reference we have (or just do nothing if FObject is nil(
      FObject := value;  
    end;

Then you use it like this:

    Procedure doSomething(obj : TFslObject);
    var
      s : TSomeObject;
    begin
      s := TSomeObject.create;
      try
        s.object_ := obj.link; // as long as s lives, it keeps a copy of s.
        // do stuff with s
      finally
        s.free;
      end;
    end;

And in the TSomeObject Destructor:

    Destructor TSomeObject.destroy;
    begin
      // other stuff
      Object_ := nil; // equivalent to freeandNil(Object);
      inherited;
    end;

Note: this pattern arose experimentally seeking a reproducible
pattern that would make it easy to keep objects alive when you
need them, and not have any leaks, in a large programming team.
Once you're used to it, it's hard to think any other way.

