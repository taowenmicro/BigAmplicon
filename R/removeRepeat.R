#' project from NCBI, related to amplicon and env microbiome project
#'
#' @param path
#' @examples
#' removeRepeat()
#' @return data.frame
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn} Jun Yuan \email{junyuan@@njau.edu.cn} Penghao Xie \email{2019103106@@njau.edu.cn}
#' @references
#'
#' Yuan J, Zhao J, Wen T, Zhao M, Li R, Goossens P, Huang Q, Bai Y, Vivanco JM, Kowalchuk GA, Berendsen RL, Shen Q
#' Root exudates drive the soil-borne legacy of aboveground pathogen infection
#' Microbiome 2018,DOI: \url{doi: 10.1186/s40168-018-0537-x}
#' @export
#---20200916版本
removeRepeatold <- function(path){

  #--merge repead file to single project
  fl_1 =dir(path, pattern = c(""), full.names = TRUE, ignore.case = TRUE)
  A = sapply(strsplit(gsub("=","-",fl_1 ), "-"), `[`, 1)
  A = sapply(strsplit(A, "/"), `[`, length(strsplit(A, "/")[[1]]))
  A = gsub(".over","",A)
  A
  data = as.data.frame(table(A))
  #-
  pro = as.character(data[data $Freq > 1,]$A)
  pro
  # i = 1
  if (length(pro) != 0) {
    for (i in 1:length(pro)) {
      fl_rep = dir(path, pattern = c(pro[i]), full.names = TRUE, ignore.case = TRUE)
      # j = 1
      sourcefile = c()
      for (j in 1:length(fl_rep)) {
        temp_i = dir(fl_rep[j], pattern = c("SRR_Acc_List"), full.names = TRUE, ignore.case = TRUE)
        if (length(temp_i ) == 0) {
          sourcefile  = c(sourcefile,fl_rep[j])
        }
        if (length(temp_i ) != 0) {
          sinkfile = fl_rep[j]
        }

      }
      # print( sinkfile)
      # print(sourcefile)
      #
      sourcefile_i = dir(sourcefile, pattern = c("SRA"), full.names = TRUE, ignore.case = TRUE)
      sourcefile_ii = dir(sourcefile_i, pattern = c(""), full.names = TRUE, ignore.case = TRUE)
      sourcefile
      file_move(sourcefile_ii,paste(sinkfile,"/SRA",sep = ""))
      size <- dir_info(path =sourcefile, recurse = TRUE) %>%
        # group_by(directory = path_dir(path)) %>%
        tally(wt = size, sort = TRUE)
      if (as.numeric(gsub("K","",size$n)) < 1000) {
        unlink(dir(sourcefile, pattern = c("SRA"), full.names = TRUE, ignore.case = TRUE), recursive=TRUE)
        unlink(sourcefile, recursive=TRUE)
        print("remave")
      } else if (as.numeric(gsub("M","",size$n)) < 1) {
        unlink(dir(sourcefile, pattern = c("SRA"), full.names = TRUE, ignore.case = TRUE), recursive=TRUE)
      }
    }
  }

}

#---20200917版本
#这个版本想要解决项目文件夹内部重复的问题，但是目前没有写完，暂时功能
#只是比第一版本强大一点
#
removeRepeat <- function(path){

  #--merge repead file to single project
  fl_1 =dir(path, pattern = c(""), full.names = TRUE, ignore.case = TRUE)
  A = sapply(strsplit(gsub("=","-",fl_1 ), "-"), `[`, 1)
  A = sapply(strsplit(A, "/"), `[`, length(strsplit(A, "/")[[1]]))
  A = gsub(".over","",A)
  A
  data = as.data.frame(table(A))
  #-
  pro = as.character(data[data $Freq > 1,]$A)
  pro
  # i = 1
  if (length(pro) != 0) {
    for (i in 1:length(pro)) {
      fl_rep = dir(path, pattern = c(pro[i]), full.names = TRUE, ignore.case = TRUE)
      # j = 1
      sourcefile = c()
      for (j in 1:length(fl_rep)) {
        temp_i = dir(fl_rep[j], pattern = c("SRR_Acc_List"), full.names = TRUE, ignore.case = TRUE)
        if (length(temp_i ) == 0) {
          sourcefile  = c(sourcefile,fl_rep[j])
        }
        if (length(temp_i ) != 0) {
          sinkfile = fl_rep[j]
        }

      }
      # print( sinkfile)
      # print(sourcefile)
      #
      sourcefile_i = dir(sourcefile, pattern = c("SRA"), full.names = TRUE, ignore.case = TRUE)
      sourcefile_ii = dir(sourcefile_i, pattern = c(""), full.names = TRUE, ignore.case = TRUE)
      sourcefile
      file_move(sourcefile_ii,paste(sinkfile,"/SRA",sep = ""))
      size <- dir_info(path =sourcefile, recurse = TRUE) %>%
        # group_by(directory = path_dir(path)) %>%
        tally(wt = size, sort = TRUE)
      !is.na(as.numeric(gsub("K","",size$n)))
      if (is.na(as.numeric(gsub("K","",size$n)))) {
        if (!is.na(as.numeric(gsub("M","",size$n)))) {
          if (as.numeric(gsub("M","",size$n)) < 1) {
            unlink(dir(sourcefile, pattern = c("SRA"), full.names = TRUE, ignore.case = TRUE), recursive=TRUE)
          } else {
            unlink(dir(sourcefile, pattern = c(""), full.names = TRUE, ignore.case = TRUE), recursive=TRUE)
            # tempfile= dir(path, pattern = c(pro[i]), full.names = FALSE, ignore.case = TRUE)
            # A = sapply(strsplit(gsub("=","-",tempfile[1]), "-"), `[`, 1)
            # A = sapply(strsplit(A, "/"), `[`, length(strsplit(A, "/")[[1]]))
            # A = gsub(".over","",A)
            # A
            # pathsub = dir(sourcefile, pattern = c(A), full.names = TRUE, ignore.case = TRUE)
            # A = gsub(".over","",pathsub)
            # A = sapply(strsplit(A, "/"), `[`, length(strsplit(A, "/")[[1]]) - 1)
            # merge_file(paste(path,A,sep = "/"),info_path)
            #
            # unlink(dir(sourcefile, pattern = c(A), full.names = TRUE, ignore.case = TRUE), recursive=TRUE)
          }
        }

      }else if(as.numeric(gsub("K","",size$n)) < 1000)  {
        unlink(dir(sourcefile, pattern = c("SRA"), full.names = TRUE, ignore.case = TRUE), recursive=TRUE)
        unlink(sourcefile, recursive=TRUE)
        print("remave")
      }



    }
  }

}

