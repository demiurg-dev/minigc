use std::borrow::Borrow;
use std::hash::{BuildHasher, Hash};
use std::rc::Rc;

use hashbrown::HashMap;
use hashbrown::hash_map::RawEntryMut;

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct Name {
    inner: Rc<String>,
}

impl Name {
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.inner
    }
}

#[derive(Clone, Default)]
pub(crate) struct Names {
    map: HashMap<NameInternal, ()>,
}

impl Names {
    pub fn get_str(&mut self, name: &str) -> Name {
        self.map.entry_ref(name).insert(()).key().into()
    }

    #[allow(dead_code)]
    pub fn get_string(&mut self, name: String) -> Name {
        // Note: this low-level optimization is not necessary, but it's done for
        // demonstration purpose
        let hash = self.map.hasher().hash_one(&name);
        match self
            .map
            .raw_entry_mut()
            .from_key_hashed_nocheck(hash, &name)
        {
            RawEntryMut::Occupied(entry) => entry.key().into(),
            RawEntryMut::Vacant(entry) => (&*entry.insert_hashed_nocheck(hash, name.into(), ()).0).into(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct NameInternal {
    inner: Rc<String>,
}

impl From<&NameInternal> for Name {
    fn from(value: &NameInternal) -> Self {
        Self { inner: value.inner.clone() }
    }
}

impl From<&str> for NameInternal {
    fn from(value: &str) -> Self {
        NameInternal { inner: Rc::new(value.to_owned()) }
    }
}

impl From<String> for NameInternal {
    fn from(value: String) -> Self {
        NameInternal { inner: Rc::new(value) }
    }
}

impl Borrow<str> for NameInternal {
    fn borrow(&self) -> &str {
        &self.inner
    }
}

impl Borrow<String> for NameInternal {
    fn borrow(&self) -> &String {
        &self.inner
    }
}

impl Borrow<str> for Name {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

impl std::fmt::Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}
