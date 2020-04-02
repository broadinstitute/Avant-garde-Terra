workflow Avantgarde {

    call convert_csv_to_zipped_parquet
    scatter (one_zip in convert_csv_to_zipped_parquet.output_zip) {
        call unzip_csv_avg {input: zip_file = one_zip, glossary_file = convert_csv_to_zipped_parquet.output_glossary}
    }
    call final_r_reports {input: csvs = flatten(unzip_csv_avg.output_csvs)}
}

task convert_csv_to_zipped_parquet {

    File inputcsv
    Int chunk_size
    Int num_vm

    String docker_image_name
    String mem_size
    Int disk_size

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
    }

    runtime {
        docker: docker_image_name
        memory: mem_size
        disks: "local-disk " + disk_size + " SSD"
    }
}

task unzip_csv_avg {

    File zip_file
    File params_file
    File glossary_file

    String docker_image_name
    String mem_size
    Int disk_size

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
    #from .luigi_avg_rtask_utils import generate_subprocess_call_for_a_analyte, run_r_script_for_an_analyte, run_r_script_for_all_analytes

    #print working directory
    print(os.getcwd())

    os.mkdir("zip_output")
    os.mkdir("csvs")
    unzip_ParquetPartition_keepingDatasetstructure(
            zip_filepath="${zip_file}", 
            zip_output_path="zip_output")

    new_path = create_path_of_PQpartition("zip_output")

    parquet_to_csvs_from_one_partition(new_path, new_path, "csvs")

    print(os.listdir("csvs"))

    print("**************************")

    x = pd.read_csv("${glossary_file}")
    print(x.head())

    print("**************************")

    os.mkdir("avg_results")

    def generate_subprocess_call_for_a_analyte(hashed_id, csv_ds_root_path, params_file_path, output_dir):

        R_SCRIPT_PATH = "Rscript"
        local_path = "/usr/local/src/"

        subprocess_call_for_r_script = str(
            R_SCRIPT_PATH +
            ' "' + os.path.join(local_path,"scatter_dummy_file_test.R") + '" ' +
            ' "' + os.path.join(local_path, csv_ds_root_path, 'data_analyte_' + str(hashed_id) + '.csv') + '" ' +
            ' "' + str(params_file_path) + '" ' +
            ' "' + str(hashed_id) + '" ' +
            ' "' + os.path.join(local_path, output_dir) + '" ')

        return subprocess_call_for_r_script

    def run_r_script_for_an_analyte(hashed_id, csv_ds_root_path, params_file_path, output_dir):

        subprocess_call_for_r_script = generate_subprocess_call_for_a_analyte(
            hashed_id, csv_ds_root_path, params_file_path, output_dir)

        print('subcall:' + subprocess_call_for_r_script)

        subprocess.call(subprocess_call_for_r_script, shell=True)  

    run_r_script_for_an_analyte(hashed_id = "0fe30dcd", csv_ds_root_path = "csvs", params_file_path="${params_file}", output_dir="avg_results")    

    CODE
    >>>

    output {
        Array[File] output_csvs = glob('csvs/*.csv')
    }

    runtime {
        docker: docker_image_name
        memory: mem_size
        disks: "local-disk " + disk_size + " SSD"
    }
}

task final_r_reports {

    Array[File] csvs

    String docker_image_name
    String mem_size
    Int disk_size

    command {
        Rscript /usr/local/src/dummy_gather.R "${sep=' ' csvs}"
    }

    output {
        File final_output = 'final_result/all_csv.csv'
    }

    runtime {
        docker: docker_image_name
        memory: mem_size
        disks: "local-disk " + disk_size + " SSD"
    }
}