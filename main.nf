#!/usr/bin/env nextflow



process CVD_ANALYSIS {
  //  container "${params.container}"
    label "charlie"

    input:
    path csv_file

    output:
    path risk
    script:
      risk= csv_file.simpleName+"_risk.csv"
      """
      cvd_risk_analysis.R ${csv_file} $risk
      """
}

process CVD_PLOTS{
     label "charlie"
     input:
	path risk
	
     output:
	path plot
	
     script:
       plot= risk.simpleName+".pdf"


       """
       plot_risk.R ${risk} $plot
       """
}


workflow {
    main:
    Channel.fromPath(params.input)
        | CVD_ANALYSIS | CVD_PLOTS

	
    publish:
       plot=CVD_PLOTS.out
}

output{
      plot {
         path {'plots'}
        }

}
