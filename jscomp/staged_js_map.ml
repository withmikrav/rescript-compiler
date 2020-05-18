


let extract (types : Parsetree.structure_item)  = 
    match types.pstr_desc with 
    | Pstr_type (_rec,tdcls) -> tdcls
    | _ -> assert false