#' Example Multihit Cluster Table from AAVengeR
#'
#' An example multihit clusters RDS output file produced by AAVengeR
#'
#' @format A tibble with 6 rows and 9 columns:
#' \describe{
#'   \item{trial}{Character, identifier for the trial in which the data was collected (e.g., sequencing run ID).}
#'   \item{subject}{Character, identifier for the subject or individual in the trial (e.g., sample1).}
#'   \item{sample}{Character, identifier for the specific sample collected from the subject.}
#'   \item{clusterID}{Character, unique identifier for each read cluster within the sample.}
#'   \item{nodes}{Integer, number of unique nodes (reads) that constitute each cluster.}
#'   \item{reads}{Integer, total number of reads associated with each cluster.}
#'   \item{UMIs}{Integer, count of unique molecular identifiers (UMIs) observed within each cluster.}
#'   \item{posids}{List of characters, positional identifiers within the genome or sequence for each cluster, with the number of elements corresponding to the `nodes` count.}
#'   \item{readIDs}{List of characters, identifiers for each read in the cluster, with the number of elements corresponding to the `reads` count.}
#' }
#'
#' @details This dataset is used for downstream analysis in AAVenger workflows, particularly for clustering scenarios where unique molecular identifiers and positional data provide insights into sequence diversity and read redundancy.
#'
#' @source Generated data from an AAVengeR run.
'example_aavenger_multihitcluster_table'
