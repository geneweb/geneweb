include Gwdb1_internal

module ToGwdb = struct
  let base : Gwdb1_internal.base -> Gwdb.base = Obj.magic
  let person : Gwdb1_internal.person -> Gwdb.person = Obj.magic
  let iper : Gwdb1_internal.iper -> Gwdb.iper = Obj.magic
end

module OfGwdb = struct
  let base : Gwdb.base -> Gwdb1_internal.base = Obj.magic
  let person :  Gwdb.person -> Gwdb1_internal.person = Obj.magic
  let iper : Gwdb.iper -> Gwdb1_internal.iper = Obj.magic
end
