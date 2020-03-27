workflow Avantgarde {

    call convert_csv_to_zipped_parquet
    scatter (one_zip in convert_csv_to_zipped_parquet.output_zip) {
        call zipped_parquet_to_csv {input: zip_file = one_zip}
    }
    scatter()
    call final_r_reports {input: csvs = flatten(zipped_parquet_to_csv.output_csvs)}
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
    }

    runtime {
        docker: docker_image_name
        memory: mem_size
        disks: "local-disk " + disk_size + " SSD"
    }
}

task zipped_parquet_to_csv {

    File zip_file

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

    os.mkdir("zip_output")
    os.mkdir("csvs")
    unzip_ParquetPartition_keepingDatasetstructure(
            zip_filepath="${zip_file}", 
            zip_output_path="zip_output")

    def create_path_of_PQpartition(path):
        a = os.listdir(path)[0]
        b = os.path.join(path, a)
        return b

    new_path = create_path_of_PQpartition("zip_output")

    def parquet_to_csvs_from_one_partition(path_PQ_partition_byGroup, parquet_dataset_dirpath,output_dirpath):
        dd = [w.replace('ID_Analyte=', '') for w in os.listdir(path_PQ_partition_byGroup)]
        dd = pd.DataFrame(dd, columns=['ID_Analyte'])
        dd['ID_Analyte'].map(lambda x: read_only_one_partition_and_write_csv(
            parquet_dataset_dirpath=parquet_dataset_dirpath,
            output_dirpath=output_dirpath,
            ID_analyte=x))

    parquet_to_csvs_from_one_partition(new_path, new_path, "csvs")

    #run R task

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


# for later with Avant-garde script:
#    command {
#    Rscript /path/ --options
#    }

task final_r_reports {

    Array[File] csvs

    String docker_image_name
    String mem_size
    Int disk_size

    command<<<
    python3 <<CODE
    import os
    import pandas as pd

    os.mkdir("final_result")

    all_csv = pd.concat([pd.read_csv(f) for f in "${sep=' ' csvs}".split()])
    all_csv.to_csv("final_result/all_csv.csv", index=False)

    CODE
    >>>

    output {
        File final_output = 'final_result/all_csv.csv'
    }

    runtime {
        docker: docker_image_name
        memory: mem_size
        disks: "local-disk " + disk_size + " SSD"
    }
}

task Run_R_Task {
    File file_list
    File params_file

    String docker_image_name
    String mem_size
    Int disk_size

    command<<<
    python3 <<CODE
    run_r_script_for_all_analytes(id_analyte_path=input_path,
                                      csv_ds_root_path=self.csv_ds_root_path,
                                      params_file_path=params_file_path,
                                      output_dir=self.output_dir)

    df = pd.read_csv(input_path)
    df.to_csv(self.output().path, index=False)
    CODE
    >>>

}