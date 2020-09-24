#' project from NCBI, related to amplicon and env microbiome project
#'
#' @param path
#' @examples
#' merge_file()
#' @return data.frame
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn} Jun Yuan \email{junyuan@@njau.edu.cn} Penghao Xie \email{2019103106@@njau.edu.cn}
#' @references
#'
#' Yuan J, Zhao J, Wen T, Zhao M, Li R, Goossens P, Huang Q, Bai Y, Vivanco JM, Kowalchuk GA, Berendsen RL, Shen Q
#' Root exudates drive the soil-borne legacy of aboveground pathogen infection
#' Microbiome 2018,DOI: \url{doi: 10.1186/s40168-018-0537-x}
#' @export
# 这部分是新的sratool（2.10以上）需要做的工作-合并sra下载文件到同一个文件中
merge_file <- function(path,info_path){
  unlink(dir(path, pattern = c("SRA"), full.names = TRUE, ignore.case = TRUE), recursive=TRUE)

  fl_1 =dir(path, pattern = c(""), full.names = TRUE, ignore.case = TRUE)
  fl_1



  for (i in 1:length(fl_1 )) {
    fl_i =dir(fl_1[i], pattern = c(""), full.names = F, ignore.case = TRUE)
    fl_i
    A = sapply(strsplit(gsub("=","-",fl_1 [i]), "-"), `[`, 1)
    A = sapply(strsplit(A, "/"), `[`, length(strsplit(A, "/")[[1]]))
    A = gsub(".over","",A)
    A
    orgname <- as.character(read.delim(paste(info_path,A,"SRR_Acc_List.txt",sep = "/"),header = FALSE)$V1)
    orgname

    a = paste(fl_1[i],intersect(fl_i,orgname),paste(intersect(fl_i,orgname),".sra",sep = ""),sep = "/")
    b = as.data.frame(file_exists(a))
    a = a[b$`file_exists(a)`]
    a
    dir.create(paste(fl_1[i],"/SRA",sep = ""))
    file_move(a,paste(fl_1[i],"/SRA",sep = ""))
    if (length(intersect(fl_i,orgname)) != 0 ) {
      unlink(paste(fl_1[i],intersect(fl_i,orgname),sep = "/"), recursive=TRUE)
    }
  }

}
