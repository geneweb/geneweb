val gc : ?dry_run:bool -> Dbdisk.dsk_base -> int list * int list * int list
(** [gc ~dry_run base] launch garbage collector over that analyse database
    [base] that detects all deleted or empty elements that aren't referenced by
    anyone (unmarked element). If [dry_run] is unset then performs memory
    compacting by eliminating all unmarked elements from all database arrays and
    update corresponding database on the disk. Otherwise, it just perform
    computing stage without database update. Returns
    [(deletedp,deletedf,deleteds)] where [deletedp] is ids of all unmarked
    persons, [deletedf] is ids of all unmarked families and [deleteds] is ids of
    all unmarked strings *)
