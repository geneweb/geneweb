open Config

val print : config -> Geneweb_db.Driver.base -> unit
(** [print conf base] handles the [m=UPD] request: in-place update of a single
    field of a person ([i=<iper>]) or of one of their families
    ([i=<iper>&f=<ifam>]), answering in JSON ([Content-type: application/json],
    no [Output.status] call, following the {!CheckDataDisplay} convention so the
    client's [response.ok] holds). A missing or malformed [i]/[f] yields the
    standard incorrect-request page; every other outcome is JSON.

    The edited field is the first request parameter whose key matches a known
    field name. Person scalar fields: key components [first_name], [surname],
    [occ] (0..99999); plain text [public_name]; places [birth_place],
    [baptism_place], [death_place], [burial_place] (in-place text);
    wiki-rendered [occu], [psources], [birth_src], [baptism_src], [death_src],
    [burial_src]. Person list fields, one element at a time through an indexed
    key (0-based, e.g. [alias1] for the second alias): [qualifier], [alias],
    [first_name_alias], [surname_alias]. Family fields: [marriage_place] and
    [marriage_src] (written into the family event they derive from; an error if
    no such event exists) and [fsources].

    The value goes through {!Util.only_printable}. An empty value, a value equal
    to the current one, an unknown field, an invalid [occ] or an out-of-range
    list index are no-ops: [{success:false, message:""}]; fields cannot be
    emptied in place. Forbidden characters are silently purged from [first_name]
    and [surname], as the regular form does.

    Persistence reuses the regular pipelines: {!UpdateIndOk.effective_mod} (with
    [~skip_conflict] and a [~prerr] that raises instead of printing HTML),
    notes-link renaming on key change, {!Util.commit_patches} and the history
    record; family edits go through {!UpdateFamOk.effective_mod}. A duplicate
    key, a failed check or a stale field [{success:false, message}].

    Concurrency: a per-field compare-and-swap. The client echoes the raw value
    it is replacing in [prev]; the edit is applied only if [prev] equals the
    current stored value of the field (an unrelated field changed by another
    wizard never blocks it), otherwise [{success:false, message}] with
    {!Update.UERR_digest}. The first edit is covered like any other, [prev]
    being intrinsic to the request. CSRF is handled by the wizard session token
    carried in [henv], not here.

    Success response:
    [{success:true, label, before, after, after_html, raw, reload}]. [label] is
    built server-side from the field's lexicon key (1-based position appended
    for list elements). [before] and [raw] are the verbatim stored values,
    before and after the edit (brackets preserved for places); the diff toast
    compares them, so a change confined to the raw form — e.g. removing a
    sub-place's brackets — is reported. [after] is the display form of the new
    value, bracket-stripped through {!Util.raw_string_of_place} for places,
    injected in place of the element. [after_html], non-empty for the
    wiki-rendered fields (sources and [occu]), is the wiki rendering through
    {!Notes.wiki_of_source} — identical to the page rendering, capitalized for
    [occu] — injected instead; otherwise the element shows [after] as text.
    [raw] also reseeds the editor before any reload. [reload] is non-empty for
    key components only: the URL (built with {!Util.commd} and {!Util.acces})
    the client navigates to.

    Requests are sent as POST; GET is accepted as well. *)
