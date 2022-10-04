module X0Allocation
    open X0BuildInterferences
    open GraphColoring
    open X0Lang

    let prepareAllocation vars =
        List.map (fun vname -> (vname, X0RV(vname))) vars |> Map.ofList

    let allocateOnStack getSize stackRegister baseOffset vars =
        let refs = Map.toList vars
        let allocS, endOffset = 
            List.mapFold 
                (fun offset v ->
                    match v with
                    | _, X0RV (vname) ->
                        let size = getSize vname
                        let varRef, nextOffset = (X0RM(stackRegister, -offset), offset + size)
                        ((vname, varRef), nextOffset)
                    | _ -> (v, offset)) 
                baseOffset refs
        (allocS |> Map.ofList)

    let allocateOnRegisters getSize registers varMap vars =
        let usedRegs = 
            Map.values vars 
            |> List.ofSeq 
            |> List.choose (fun ref -> match ref with | X0RR(r) -> Some(r) | _ -> None)
            |> Set.ofList            
        let availableRegs = Set.difference registers usedRegs
        let regCount = Set.count availableRegs
        let regMap = 
            Set.toList availableRegs 
            |> List.mapi (fun id r -> (id, r))
            |> Map.ofList
        let translate ref =
            match ref with
            | X0RV(vname) -> 
                match Map.tryFind vname varMap with
                | Some(color) -> 
                    if color < regCount then X0RR(Map.find color regMap) 
                    else ref
                | None -> ref
            | _ -> ref
        Map.map (fun vname ref -> translate ref) vars