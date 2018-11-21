include Gwdb2_internal

module ToGwdb = struct
  let base : Gwdb2_internal.base -> Gwdb.base = Obj.magic
  let person : Gwdb2_internal.person -> Gwdb.person = Obj.magic
end

module OfGwdb = struct
  let base : Gwdb.base -> Gwdb2_internal.base = Obj.magic
  let person :  Gwdb.person -> Gwdb2_internal.person = Obj.magic
end
