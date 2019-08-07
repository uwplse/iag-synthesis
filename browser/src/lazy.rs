use std::borrow::Borrow;
use std::cell::UnsafeCell;
use std::convert::TryInto;
use std::convert::{AsMut, AsRef};
use std::ops::Deref;
use std::rc::Rc;

/// A lazily initialized smart pointer to a potentially shared immutable value.
///
/// A `Lazy<T>` smart pointer only (re-)allocates memory on the heap on
/// (re-)initialization. If never initialized, a `Lazy<T>` smart pointer will
/// automatically initialize itself on first dereference to the `Default` value
/// for `T`.
///
/// Cloning an initialized `Lazy<T>` smart pointer creates an alias with
/// internal reference counting. Since the mutation semantics are essentially
/// copy-on-write, such aliases are non-interfering. Even the pointee's address
/// can only change due to a (local) mutating operation; namely, `as_mut` will
/// reallocate the pointee if the original copy was aliased (shared).
///
/// The smart pointer `std::borrow::Cow` is similar to `Lazy<T>` and much
/// simpler; if sufficient for your needs, you should prefer it. The distinct
/// advantage of `Lazy<T>` is the ability to share values with dynamic lifetime
/// structure. In other words, you should use `Lazy<T>` only when lifetime
/// constraints cannot statically model the necessary pattern of value sharing.
/// More or less, `Lazy<T>` is the fusion of `Cow<T>` (copy-on-write sharing)
/// and `Rc<T>` (dynamic lifetimes via reference counting).
pub struct Lazy<T: ?Sized + Default + Clone>(UnsafeCell<Option<Rc<T>>>);

impl<T: ?Sized + Default + Clone> Lazy<T> {
    /// Create a new lazy pointer eagerly initialized with `val`.
    pub fn new(val: T) -> Self {
        Lazy(UnsafeCell::new(Some(Rc::new(val))))
    }

    /// Create a new lazy pointer directly initialized by a pre-exisiting
    /// reference-counted pointer.
    ///
    /// Beware of reference cycles when creating lazy pointers this way.
    pub fn share(ptr: Rc<T>) -> Self {
        Lazy(UnsafeCell::new(Some(ptr)))
    }

    /// Create a new lazy pointer lazily initialized with `T::default()`.
    pub fn nil() -> Self {
        Lazy(UnsafeCell::new(None))
    }

    /// Alias the given lazy pointer.
    ///
    /// Same as `Clone::clone`.
    pub fn dup(this: &Self) -> Self {
        unsafe { Lazy(UnsafeCell::new(this.raw().as_ref().map(Rc::clone))) }
    }

    /// Force the default initialization of the given lazy pointer, if it has
    /// not yet been forced to initialize.
    ///
    /// Equivalent to `Lazy::init(this, T::default())` of `Lazy::as_ref(this)`.
    pub fn force(this: &mut Self) {
        let _ = Lazy::as_ref(this);
    }

    /// (Re-)Initialize the given lazy pointer with `val`, allocating fresh
    /// heap memory if necessary but preferentially overwriting its original
    /// heap memory if never aliased (shared) with other lazy pointers.
    pub fn init(this: &mut Self, val: T) {
        unsafe {
            let ptr = this.raw();
            if let Some(v_ptr) = ptr.as_mut().and_then(Rc::get_mut) {
                *v_ptr = val;
            } else {
                *ptr = Some(Rc::new(val));
            }
        }
    }

    /// Get a reference to the given lazy pointer's initialized value.
    ///
    /// Forces initialization to `T::default()` if the lazy pointer has not
    /// yet been forced to initialize (i.e., if `Lazy::is_nil` holds).
    ///
    /// Note that this operation will not always be fast, since forcing lazy
    /// initialization entails heap allocation (plus default initialization).
    ///
    /// Same as `AsRef::as_ref()`, `Deref::deref()`, and `Borrow::borrow()`.
    pub fn as_ref(this: &Self) -> &T {
        unsafe { Rc::deref(this.ptr()) }
    }

    /// Get a *mutable* reference to the given lazy pointer's initialized
    /// value, reallocated if necessary to avoid alias interference.
    ///
    /// Forces initialization to `T::default()` if the lazy pointer has not
    /// yet been forced to initialize (i.e., if `Lazy::is_nil` holds).
    ///
    /// Note that this operation will not always be fast, since forcing lazy
    /// initialization entails heap allocation (plus default initialization).
    ///
    /// When one intends to overwrite the whole pointee value, using
    /// `Lazy::init()` instead of this operation will avoid wasted default/copy
    /// initialization.
    ///
    /// Same as `AsMut::as_mut()`.
    pub fn as_mut(this: &mut Self) -> &mut T {
        unsafe { Rc::make_mut(this.ptr()) }
    }

    /// Get an alias of the internal reference-counting pointer, if present.
    ///
    /// Beware of reference cycles when using the returned pointer externally.
    ///
    /// Same as `TryInto<Rc<T>>::try_into()`, modulo `Option` vs. `Result`.
    pub fn as_ptr(this: &Self) -> Option<Rc<T>> {
        unsafe { this.raw().as_ref().map(Rc::clone) }
    }

    /// Has this lazy pointer been yet forced to initialize?
    pub fn is_nil(this: &Self) -> bool {
        unsafe { this.raw().is_none() }
    }

    /// Is this lazy pointer sharing its pointee with an alias?
    ///
    /// Inverse of `Lazy::is_unique()`.
    pub fn is_shared(this: &Self) -> bool {
        Lazy::share_count(this) > 1
    }

    /// Is this lazy pointer the only one using its initialized pointee?
    ///
    /// Inverse of `Lazy::is_shared()`.
    pub fn is_unique(this: &Self) -> bool {
        Lazy::share_count(this) == 1
    }

    /// Get the current number of shared users for the given lazy pointer's
    /// allocated value (pointee), as determined by the internal ref-counting
    /// pointer.
    ///
    /// Returns zero if this lazy pointer has not yet been forced to initialize
    /// (i.e., if `Lazy::is_nil` holds).
    ///
    /// Note that weak aliases, which are never created by lazy pointers
    /// themselves, are omitted from this count.
    pub fn share_count(this: &Self) -> usize {
        unsafe { this.raw().as_ref().map_or(0, Rc::strong_count) }
    }

    /// Get a mutable reference to the (non-null) ref-counting pointer inside
    /// the `UnsafeCell` and `Option` (encoding the potential null), allocating
    /// and default-initializing such a ref-counting pointer if absent.
    unsafe fn ptr(&self) -> &mut Rc<T> {
        self.raw().get_or_insert_with(|| Rc::new(T::default()))
    }

    /// Get a mutable reference to the optional (effectively, nullable)
    /// ref-counting pointer inside the `UnsafeCell`.
    unsafe fn raw(&self) -> &mut Option<Rc<T>> {
        self.0.get().as_mut().unwrap()
    }
}

impl<T: ?Sized + Default + Clone> Default for Lazy<T> {
    fn default() -> Self {
        Lazy::nil()
    }
}

impl<T: ?Sized + Default + Clone> Clone for Lazy<T> {
    fn clone(&self) -> Self {
        Lazy::dup(self)
    }
}

impl<T: ?Sized + Default + Clone> Deref for Lazy<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        Lazy::as_ref(self)
    }
}

impl<T: ?Sized + Default + Clone> AsRef<T> for Lazy<T> {
    fn as_ref(&self) -> &T {
        Lazy::as_ref(self)
    }
}

impl<T: ?Sized + Default + Clone> AsMut<T> for Lazy<T> {
    fn as_mut(&mut self) -> &mut T {
        Lazy::as_mut(self)
    }
}

impl<T: ?Sized + Default + Clone> Borrow<T> for Lazy<T> {
    fn borrow(&self) -> &T {
        self.as_ref()
    }
}

impl<T: ?Sized + Default + Clone> From<Rc<T>> for Lazy<T> {
    fn from(ptr: Rc<T>) -> Self {
        Lazy::share(ptr)
    }
}

impl<T: ?Sized + Default + Clone> TryInto<Rc<T>> for &Lazy<T> {
    type Error = ();

    fn try_into(self) -> Result<Rc<T>, ()> {
        Lazy::as_ptr(self).ok_or(())
    }
}

/* Here we punt an assortment of standard traits through the indirection... */

impl<T: ?Sized + Default + Clone + std::fmt::Debug> std::fmt::Debug for Lazy<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if Lazy::is_nil(self) {
            T::default().fmt(f)
        } else {
            self.as_ref().fmt(f)
        }
    }
}

impl<T: ?Sized + Default + Clone + PartialEq> PartialEq for Lazy<T> {
    fn eq(&self, other: &Self) -> bool {
        let default = T::default();
        let left = if Lazy::is_nil(self) {
            &default
        } else {
            self.as_ref()
        };
        let right = if Lazy::is_nil(other) {
            &default
        } else {
            other.as_ref()
        };
        T::eq(left, right)
    }
}

impl<T: ?Sized + Default + Clone + Eq> Eq for Lazy<T> {}

impl<T: ?Sized + Default + Clone + PartialOrd> PartialOrd for Lazy<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let default = T::default();
        let left = if Lazy::is_nil(self) {
            &default
        } else {
            self.as_ref()
        };
        let right = if Lazy::is_nil(other) {
            &default
        } else {
            other.as_ref()
        };
        T::partial_cmp(left, right)
    }
}

impl<T: ?Sized + Default + Clone + Ord> Ord for Lazy<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let default = T::default();
        let left = if Lazy::is_nil(self) {
            &default
        } else {
            self.as_ref()
        };
        let right = if Lazy::is_nil(other) {
            &default
        } else {
            other.as_ref()
        };
        T::cmp(left, right)
    }
}
