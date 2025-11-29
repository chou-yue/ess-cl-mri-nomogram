# Study Flowchart Generation
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

flowchart <- grViz("
digraph flowchart {
  # Graph properties
  graph [layout = dot, rankdir = TB, bgcolor = white, splines = ortho]
  
  # Unified node properties
  node [shape = box, fontname = 'Arial', fontsize = 11, color = black, fontcolor = black, penwidth = 2]
  
  # Flowchart main body
  
  # Starting node
  start [label = 'Study Design: Retrospective Dual-Center\\nDiagnostic Study for MRI\\nDifferentiation of ESS vs CL']
  
  # Derivation cohort start
  cohortA_start [label = 'Derivation Cohort (Hospital A)\\nMarch 2016 - March 2025\\nESS: N=284, CL: N=673\\nTotal N=957', 
                 width = 3.2]
  
  # Exclusion: No preoperative MRI
  exclude_no_mri [label = 'Excluded\\nNo preoperative pelvic MRI\\nESS: N=224, CL: N=565\\nTotal N=789', style = dashed]
  
  # Patients with preoperative MRI
  mri_available [label = 'Patients with preoperative\\npelvic MRI examination\\nESS: N=60, CL: N=108\\nTotal N=168']
  
  exclude_3t [label = 'Excluded\\nMRI performed on 3.0T scanner\\nESS: N=11, CL: N=26\\nTotal N=37', style = dashed]
  
  scanner_1_5t [label = 'Patients scanned on 1.5T\\nESS: N=49, CL: N=82\\nTotal N=131']
  
  exclude_others [label = 'Excluded\\nESS (N=8): Prior outpatient treatment (N=1),\\nLesion diameter <1cm (N=2),\\nPoor image quality (N=5)\\n\\nCL (N=12): Uncertain diagnosis (N=4),\\nPoor image quality (N=6),\\nLesion diameter <1cm (N=2)\\nTotal N=20', style = dashed]
  
  cohortA_final [label = 'Derivation Cohort Final\\nESS: N=41, CL: N=70\\nTotal N=111']
  
  # External validation cohort subgraph
  subgraph cluster_B {
    label = 'External Validation Cohort (Hospital B)';
    fontname = 'Arial';
    
    cohortB_ess_start [label = 'Pathologically confirmed\\nESS cases retrieved\\n(N=20)'];
    cohortB_cl_start [label = 'CL cases retrieved from\\npathology database\\n(Jan 2019 - Dec 2021)'];
    exclude_external_B [label = 'Excluded (N=4)\\n- Poor image quality (N=2)\\n- Lesion diameter <1cm (N=2)', style = dashed];
    cohortB_cl_method [label = 'Consecutive enrollment of the first 28 cases\\nmeeting all inclusion/exclusion criteria'];
    cohortB_final_B [label = 'External Validation Cohort Final\\nESS: N=16, CL: N=28\\nTotal N=44'];
    
    # Internal connections
    cohortB_ess_start -> exclude_external_B [style=dashed];
    cohortB_ess_start -> cohortB_final_B;
    cohortB_cl_start -> cohortB_cl_method;
    cohortB_cl_method -> cohortB_final_B;
    
    # Internal hierarchy
    {rank=same; cohortB_ess_start; cohortB_cl_start;}
    {rank=same; exclude_external_B; cohortB_cl_method;}
  }
  
  # Study overview
  final_analysis [label = 'Final Study Population\\nDerivation Cohort: N=111\\nExternal Validation Cohort: N=44\\nTotal Study Population: N=155']
  
  # Global connections
  start -> cohortA_start;
  cohortA_start -> exclude_no_mri [style = dashed];
  cohortA_start -> mri_available;
  mri_available -> exclude_3t [style = dashed];
  mri_available -> scanner_1_5t;
  scanner_1_5t -> exclude_others [style = dashed];
  scanner_1_5t -> cohortA_final;
  cohortA_final -> final_analysis;
  cohortB_final_B -> final_analysis;
  
  # Global hierarchy
  {rank = same; cohortA_start; }
  {rank = same; exclude_no_mri; mri_available;}
  {rank = same; exclude_3t; scanner_1_5t;}
  {rank = same; exclude_others; cohortA_final;}
}
")

flowchart

svg_txt <- export_svg(flowchart)
writeLines(svg_txt, "flowchart.svg")
rsvg_pdf(charToRaw(svg_txt), "flowchart.pdf", width = 504)

