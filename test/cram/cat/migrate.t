We used to report on workitems and now we use objectives.

During the transition, using workitems makes the linting fail
and the error message points to the corresponding objective.

  $ mkdir -p admin/data

  $ cat > admin/data/db.csv << EOF
  > "id","title","status","quarter","team","pillar","objective","funder","labels","progress"
  > "Absence","Leave","Active 🏗","Rolling","Engineering","All","","","",""
  > "Learn","Learning","Active 🏗","Rolling","Engineering","All","","","",""
  > "Onboard","Onboard","Active 🏗","Rolling","Engineering","All","","","",""
  > "Meet","Meet","Active 🏗","Rolling","Engineering","All","","","",""
  > "#1053","Multicore OCaml Merlin project","Dropped ❌","Q3 2023 - Jul - Sep","Benchmark tooling","","Maintenance - Irmin","","",""
  > "#1058","Application and Operational Metrics","Complete ✅","Q4 2023 - Oct - Dec","Ci & Ops","QA","Operational Metrics for Core OCaml Services","Jane Street - Community","pillar/qa","50."
  > "#1090","Property-Based Testing for Multicore","Active 🏗","Q1 2024 - Jan - Mar","Compiler and language","Compiler","Property-Based Testing for Multicore","","pillar/compiler,team/compiler&language,Proposal","25."
  > "#1115","General okra maintenance","Draft","","","","Maintenance - internal tooling","","pillar/ecosystem,team/internal-tooling",""
  > EOF

  $ cat > admin/data/team-objectives.csv << EOF
  > "id","title","status","quarter","team","pillar","objective","funder","labels","progress"
  > "#543","Ensure OCaml 5 series has feature parity with OCaml 4 (Commercial)","In Progress","Q2 2024","Compiler Backend","Compiler","","Jane Street - Commercial","Proposal",""
  > "#558","Property-Based Testing for Multicore","In Progress","Q2 2024","Compiler & Language","Compiler","","","Proposal",""
  > "#677","Improve OCaml experience on Windows","Todo","Q2 2024","Multicore applications","Ecosystem","","","",""
  > "#678","Maintenance - internal tooling","In Progress","Rolling","Internal tools","Ecosystem","","","level/team",""
  > "#701","JSOO Effect Performance","","Q2 2024","Compiler & Language","Compiler","","","focus/technology,level/team",""
  > EOF

This weekly is using using workitems:

  $ cat > eng1.md << EOF
  > # Last Week
  > 
  > - Property-Based Testing for Multicore (#1090)
  >   - @eng1 (2 days)
  >   - This is a workitem with a parent objective in the DB
  > 
  > - Application and Operational Metrics (#1058)
  >   - @eng1 (1 day)
  >   - This is a workitem with no parent objective in the DB
  > 
  > - Off
  >   - @eng1 (2 days)
  > EOF

This weekly is using objectives:

  $ cat > eng2.md << EOF
  > # Last Week
  > 
  > - Property-Based Testing for Multicore (#558)
  >   - @eng2 (2 days)
  >   - This is an objective
  > 
  > - Improve OCaml experience on Windows (#677)
  >   - @eng2 (1 day)
  >   - This is an objective
  > 
  > - Off
  >   - @eng2 (2 days)
  > EOF

  $ cat eng1.md eng2.md | okra cat -C admin --engineer
  # Last Week
  
  - Application and Operational Metrics (#1058)
    - @eng1 (1 day)
    - This is a workitem with no parent objective in the DB
  
  - Property-Based Testing for Multicore (#558)
    - @eng1 (2 days), @eng2 (2 days)
    - This is a workitem with a parent objective in the DB
    - This is an objective
  
  - Improve OCaml experience on Windows (#677)
    - @eng2 (1 day)
    - This is an objective
  
  - Off
    - @eng1 (2 days), @eng2 (2 days)

# Automatic update of weekly using okra cat

  $ cat > eng1.workitems.md << EOF
  > # Last Week
  > 
  > - Property-Based Testing for Multicore (#1090)
  >   - @eng1 (3 days)
  >   - This is a workitem with a parent objective in the DB
  > 
  > - Off
  >   - @eng1 (2 days)
  > EOF

Linting of the original file fails because we used workitems

  $ okra lint -e -C admin eng1.workitems.md
  [OK]: eng1.workitems.md

We rewrite the file using okra cat

  $ okra cat -e -C admin eng1.workitems.md -o eng1.objectives.md
  $ cat eng1.objectives.md
  # Last Week
  
  - Property-Based Testing for Multicore (#558)
    - @eng1 (3 days)
    - This is a workitem with a parent objective in the DB
  
  - Off
    - @eng1 (2 days)

Linting of the produced file succeeds because we now use objectives

  $ okra lint -e -C admin eng1.objectives.md
  [OK]: eng1.objectives.md

Parentheses in the objective name:

  $ cat > eng1.md << EOF
  > # Last week
  > 
  > - Ensure OCaml 5 series has feature parity with OCaml 4 (Commercial) (#543)
  >   - @eng1 (1 day)
  >   - Something
  > 
  > - Off
  >   - @eng1 (4 days)
  >   - off
  > EOF

  $ okra cat -e -C admin eng1.md -o eng1.cat.md

  $ cat eng1.cat.md
  # Last week
  
  - Ensure OCaml 5 series has feature parity with OCaml 4 (Commercial) (#543)
    - @eng1 (1 day)
    - Something
  
  - Off
    - @eng1 (4 days)
    - off

  $ okra lint -e -C admin eng1.cat.md
  [OK]: eng1.cat.md

Mixing workitems and objectives:

  $ cat > eng1.md << EOF
  > # Last Week
  > 
  > - General okra maintenance (#1115)
  >   - @eng1 (2 days)
  >   - This is a workitem
  > EOF

  $ cat > eng2.md << EOF
  > # Last Week
  > 
  > - Maintenance - internal tooling (#678)
  >   - @eng2 (3 days)
  >   - This is an objective
  > EOF

  $ cat eng1.md eng2.md | okra cat -C admin --engineer
  # Last Week
  
  - Maintenance - internal tooling (#678)
    - @eng1 (2 days), @eng2 (3 days)
    - This is a workitem
    - This is an objective
