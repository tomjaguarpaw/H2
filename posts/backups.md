# Backups

Our current notion of backing up seems to conflate two things that
should be kept separate:

1. Implementation robustness: You don't want to lose the state of a
higher level service due to losing state in a lower level service.

2. Preservation of history: You want to be able to revert to a
previous state.

1 is probably best dealt with things like RAID and distributed
filesystems.

2 is probably best dealt with things like version control and
versioned filesystems.