include Gwdb1_internal

module ToGwdb = struct
  let base : Gwdb1_internal.base -> Gwdb.base = Obj.magic
  let person : Gwdb1_internal.person -> Gwdb.person = Obj.magic
end

module OfGwdb = struct
  let base : Gwdb.base -> Gwdb1_internal.base = Obj.magic
  let person :  Gwdb.person -> Gwdb1_internal.person = Obj.magic
end
