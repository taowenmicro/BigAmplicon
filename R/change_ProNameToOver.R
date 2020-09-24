#' project from NCBI, related to amplicon and env microbiome project
#'
#' @param path
#' @examples
#' change_ProNameToOver()
#' @return data.frame
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn} Jun Yuan \email{junyuan@@njau.edu.cn} Penghao Xie \email{2019103106@@njau.edu.cn}
#' @references
#'
#' Yuan J, Zhao J, Wen T, Zhao M, Li R, Goossens P, Huang Q, Bai Y, Vivanco JM, Kowalchuk GA, Berendsen RL, Shen Q
#' Root exudates drive the soil-borne legacy of aboveground pathogen infection
#' Microbiome 2018,DOI: \url{doi: 10.1186/s40168-018-0537-x}
#' @export
#---修改名字为统计格式:PROID-0-0-over

change_ProNameToOver <- function(path,info_path){
  fl_1 =dir(path, pattern = c(""), full.names = TRUE, ignore.case = TRUE)

  #-----------到这里我发现了问题，这是上次下载停下来之后，残留下来的样本，没有剪切到对应的文件中。

  for (i in 1: length(fl_1 )) {

    unlink(dir(paste(fl_1[i],"/SRA",sep = ""), pattern = c("tmp"), full.names = TRUE, ignore.case = TRUE), recursive=TRUE)
    unlink(dir(paste(fl_1[i],"/SRA",sep = ""), pattern = c("lock"), full.names = TRUE, ignore.case = TRUE), recursive=TRUE)

    a =dir(paste(fl_1[i],"/SRA",sep = ""), pattern = c(".sra"), full.names = FALSE, ignore.case = TRUE)
    a <- gsub(".sra","",a)
    a
    dowmnum <- length(a)

    #--原样本名
    # read.table(paste(fl_1[i],"SRR_Acc_List.txt",sep = "/"),header = TRUE)

    ab = file.info(paste(fl_1[i],"SRR_Acc_List.txt",sep = "/"))
    ab
    if (ab$size == 0) {

      unlink(fl_1[i], recursive=TRUE)
    }

    if (ab$size != 0) {
      # object.size(paste(fl_1[i],"SRR_Acc_List.txt",sep = "/")) %>% print(unit = "auto")
      A= sapply(strsplit(gsub("=","-",fl_1 [i]), "-"), `[`, 1)
      A= sapply(strsplit(A, "/"), `[`, length(strsplit(A, "/")[[1]]))
      A = gsub(".over","",A)
      orgname <- as.character(read.delim(paste(info_path,A,"SRR_Acc_List.txt",sep = "/"),header = FALSE)$V1)
      #---原始文件数量
      orgnum <- length(orgname)
      #--找到不属于这里面的样本
      diffSam <- setdiff(a,orgname)

      if (length(diffSam) != 0 ) {
        paste(fl_1[i],"/",diffSam,".sra",sep = "")
        #--新建一个文件夹，放入不属于这个项目的样本
        dir.create(paste(fl_1[i],"/other_allSample",sep = ""))

        file_move(paste(fl_1[i],"/","/SRA/",diffSam,".sra",sep = ""),paste(fl_1[i],"/other_allSample/",sep = ""))

        #--删除源位置的样本文件
        # unlink(paste(fl_1[i],"/",diffSam,".sra",sep = ""), recursive=TRUE)
      }

      #---重新估计样本数量dowmnum为样本数量
      a =dir(paste(fl_1[i],"/SRA",sep = ""), pattern = c(""), full.names = FALSE, ignore.case = TRUE)
      a <- gsub(".sra","",a)
      a
      dowmnum <- length(a)

      #---对完成之后进行重命名
      Newname = paste(sapply(strsplit(gsub("=","-",gsub(".over","-0-0-over",fl_1 [i])), "-"), `[`, 1),"-",dowmnum,"-",orgnum,"-over",sep = "")
      Newname
      #---重新命名
      file.rename(fl_1[i],Newname)
    }
  }
  print(i)
}
