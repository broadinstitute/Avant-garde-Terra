workflow Avantgarde {

    call convert_csv_to_zipped_parquet
    scatter (one_zip in convert_csv_to_zipped_parquet.output_zip) {
        call run_avg {input: zip_file = one_zip}
    }
    call final_r_reports {input: csvs = run_avg.output_zip, glossary_file = convert_csv_to_zipped_parquet.output_glossary, transition_loc = convert_csv_to_zipped_parquet.output_transition_loc, id_rep = convert_csv_to_zipped_parquet.output_rep, MetaData_PrecursorResults = convert_csv_to_zipped_parquet.output_metadata}
}

task convert_csv_to_zipped_parquet {

    File inputcsv
    Int chunk_size
    Int num_vm

    Int? mem_size
    Int? disk_size
    Int? num_preemptions

    command<<<
    python3 <<CODE
    import hashlib
    import subprocess
    import pandas as pd
    import pyarrow as pa
    import pyarrow.parquet as pq
    import os
    import csv
    import string
    import zipfile
    import numpy as np
    from avg_utils.parquet_file_formatting import read_csv_by_chunks_createindices_and_partitionPQbygroup
    from avg_utils.parquet_file_formatting import zip_ParquetPartitions_individually
    from avg_utils.parquet_file_formatting import create_indices, hashvalue_to_groupnumber_dictionary
    from avg_utils.parquet_file_formatting import zip_dir_keeping_folder_structure

    os.mkdir("pq_dataset")
    os.mkdir("indices_glossary")
    os.mkdir("zip_files")
    read_csv_by_chunks_createindices_and_partitionPQbygroup(
            input_csv_path="${inputcsv}",
            parquet_dataset_output_path="pq_dataset",
            indices_csv_output_path="indices_glossary",
            chunksize=${chunk_size},
            n_parts=${num_vm})

    zip_ParquetPartitions_individually(
            parquet_dataset_path="pq_dataset",
            zip_outputs="zip_files")

    CODE
    >>>

    output {
        Array[File] output_zip = glob("zip_files/*.zip")
        File output_glossary = "indices_glossary/ID_Analyte.csv"
        File output_transition_loc = "indices_glossary/ID_transition_locator.csv"
        File output_rep = "indices_glossary/ID_Rep.csv"
        File output_metadata = "indices_glossary/MetaData_PrecursorResults.csv"
    }

    runtime {
        docker: "gcr.io/lincs-phosphodia-2/avg-test-docker:latest"
        memory: select_first([mem_size, 4]) + "G"
        disks: "local-disk " + select_first([disk_size, 50]) + " SSD"
        preemptible : select_first ([num_preemptions, 0])
    }
}

task run_avg {

    File zip_file
    File params_file

    Int? mem_size
    Int? disk_size
    Int? num_threads
    Int? num_preemptions

    command<<<
    python3 <<CODE
    import hashlib
    import subprocess
    import pandas as pd
    import pyarrow as pa
    import pyarrow.parquet as pq
    import os
    import csv
    import string
    import zipfile
    import numpy as np
    from avg_utils.parquet_file_formatting import unzip_ParquetPartition_keepingDatasetstructure
    from avg_utils.parquet_file_formatting import read_only_one_partition_and_write_csv
    from avg_utils.parquet_file_formatting import create_path_of_PQpartition
    from avg_utils.parquet_file_formatting import parquet_to_csvs_from_one_partition
    from avg_utils.parquet_file_formatting import zip_dir_keeping_folder_structure
    from avg_utils.luigi_avg_rtask_utils import generate_subprocess_call_for_a_analyte, run_r_script_for_an_analyte, run_r_script_for_all_analytes

    os.mkdir("zip_output")
    os.mkdir("csvs")
    os.mkdir("avg_results")
    os.mkdir("new_zip_output")
    
    unzip_ParquetPartition_keepingDatasetstructure(
            zip_filepath="${zip_file}", 
            zip_output_path="zip_output")

    new_path = create_path_of_PQpartition("zip_output")

    parquet_to_csvs_from_one_partition(new_path, new_path, "csvs")

    run_r_script_for_all_analytes(csv_ds_root_path="csvs",
                                      params_file_path="${params_file}",
                                      output_dir="avg_results") 

    zip_dir_keeping_folder_structure(directory = "avg_results", zipname = "zipped_csvs.zip")

    CODE
    >>>

    output {
        File output_zip = "zipped_csvs.zip"
    }

    runtime {
        docker: "gcr.io/lincs-phosphodia-2/avg-test-docker:latest"
        memory: select_first([mem_size, 4]) + "G"
        disks: "local-disk " + select_first([disk_size, 50]) + " SSD"
        cpu: select_first ([num_threads, 4]) + ""
        preemptible : select_first ([num_preemptions, 0])
    }
}

task final_r_reports {

    Array[File] csvs
    File params_file
    File glossary_file
    File transition_loc
    File id_rep
    File MetaData_PrecursorResults
    String output_prefix

    Int? mem_size
    Int? disk_size
    Int? num_preemptions

    command {
        Rscript /usr/local/src/AvG_final_report.R "${params_file}" "${sep=' ' csvs}" "${glossary_file}" "${transition_loc}" "${id_rep}" "${MetaData_PrecursorResults}" "${output_prefix}_avg_results"

        zip -r "${output_prefix}_avg_results.zip" "${output_prefix}_avg_results"
    }

    output {
        File avg_results = "${output_prefix}_avg_results.zip"
    }

    runtime {
        docker: "gcr.io/lincs-phosphodia-2/avg-test-docker:latest"
        memory: select_first([mem_size, 4]) + "G"
        disks: "local-disk " + select_first([disk_size, 50]) + " SSD"
        preemptible : select_first ([num_preemptions, 0])
    }
}