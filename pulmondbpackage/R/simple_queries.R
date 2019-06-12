#' A simple_queries Function
#'
#' @return tables from queries


# TO DOWNLOAD: install_github("oaa97181/pulmondbpackage", subdir="pulmondbpackage")

simple_queries <- function(){

  mydb = dbConnect(MySQL(), 
                   user="guest", 
                   password="", 
                   dbname="expdata_hsapi_ipf", 
                   host="10.200.0.42")
  
  sql2='SELECT \
  sc.contrast_name, cd.condition_node_name, csr.value,  cd.cond_property_id \
  from sample_contrast as sc  \
  INNER JOIN condition_specification_of_ref_sample as csr ON sc.ref_sample_fk = csr.sample_fk \
  INNER JOIN condition_definition as cd ON csr.cond_property_fk = cd.cond_property_id \
  INNER JOIN sample as s ON sc.test_sample_fk = s.sample_id \
  INNER JOIN experiment as e ON s.experiment_fk= e.experiment_id \
  WHERE (e.experiment_access_id ="GSE1122")'

  rs2 = dbSendQuery(mydb,sql2)
  data2 = fetch(rs2, n=-1)
  View(data2)


  sql3='SELECT \
  sc.contrast_name, cd.condition_node_name, csc.delta_value, cd.cond_property_id \
  from sample_contrast as sc \
  INNER JOIN condition_specification_of_contrast as csc ON sc.contrast_id = csc.contrast_fk \
  INNER JOIN condition_definition as cd ON csc.cond_property_fk = cd.cond_property_id \
  INNER JOIN sample as s ON sc.test_sample_fk = s.sample_id \
  INNER JOIN experiment as e ON s.experiment_fk= e.experiment_id \
  where (csc.delta_value NOT IN (-1))\
  AND (e.experiment_access_id ="GSE1122")'

  rs3 = dbSendQuery(mydb,sql3)
  data3 = fetch(rs3, n=-1)
  data3= data.frame(data3)
  data3= group_by(data3,contrast_name)
  #data3= summarize(data3, first()) #produce error
  data3= summarize(data3, n())
  View(data3)

  #data4=merge(x=data2,y=data3, by="contrast_name", all=TRUE)
  #View(data4)

  dbDisconnect(mydb)
}


###################################################################################################################


#' A query w gene and id Function
#' @param gene,id arguments passed as strings and added to the query
#' @return specific tables from queries where you specified the gene name and experimet id


# TO DOWNLOAD PLS install_github("oaa97181/simpleplotpkg", subdir="simpleplotpkg")

genes_and_expids = function(gene, id){

  empty='this is gonna be erased'
  if(length(gene)>1){
    geneOR="OR gn.gene_name ="
    for (g in 1:((length(gene)))){
      gene2 = paste(shQuote(gene),geneOR)
    }   
    gene=gsub(",", "", toString(gene2))
    gene=paste(gene,empty)
    gene=gsub('OR gn.gene_name = this is gonna be erased', '', toString(gene))
    #gene=substring(toString(gene), first = 1, last = 3)
  } else{
    singledoublequote='"'
    gene=paste(singledoublequote,gene)
    gene=paste(gene,singledoublequote)
    gene=gsub(" ", "", gene, fixed = TRUE)
  }
  
  if(length(id)>1){
    idOR="OR e.experiment_access_id="
    for (i in 1:length(id)){
      id2 = paste(shQuote(id),idOR)
    }   
    #print(id2)
    id=gsub(",", "", toString(id2))
    id=paste(id,empty)
    id=gsub('OR e.experiment_access_id= this is gonna be erased', "", toString(id))
  }else{
    singledoublequote='"'
    id=paste(singledoublequote,id)
    id=paste(id,singledoublequote)
    id=gsub(" ", "", id, fixed = TRUE)
  }


  mydb = dbConnect(MySQL(), 
                   user="guest", 
                   password="", 
                   dbname="expdata_hsapi_ipf", 
                   host="10.200.0.42")
  
  sql = 'select \
  n.value, gn.gene_name,sc.contrast_name \
  from norm_data AS n \
  INNER JOIN gene_name as gn ON n.gene_fk=gn.gene_fk \
  INNER JOIN value_type AS vt ON n.value_type_fk=vt.value_type_id \
  INNER JOIN sample_contrast AS sc ON n.contrast_fk=sc.contrast_id \
  INNER JOIN sample AS s ON sc.test_sample_fk=s.sample_id \
  INNER JOIN experiment AS e ON s.experiment_fk= e.experiment_id  
  WHERE vt.value_type="M" AND gn.gene_name=' 
  sqlgene ='AND e.experiment_access_id='
 
  
  finalsql=paste(sql,gene,sqlgene,id)
  cat(finalsql)
  rs = dbSendQuery(mydb,finalsql)
  data = fetch(rs, n=-1)
  contrast_name_vs_gene_name = data
  #contrast_name_vs_gene_name[1] <- NULL 
  #View(data)
  df<-data.frame(data)
  #tidyr::spread(df, contrast_name, value)
  View(contrast_name_vs_gene_name)
  
  dbDisconnect(mydb)
  
}


genes_and_expids(c("MMP1","HHIP"),c("GSE52463","GSE27597"))

# site_gene =["MMP1","KRT17","SPP1","S100A2","SULF1","ALDH1A3","HHIP","SDR16C5","COMP","THY1","PTPRB","OGN","COL14A1","TMEM45A","CDH3","CXCL12","IGFBP2","CFH","GPIHBP1","CYP24A1","FNDC1","EDNRB","LAMP3","SLC39A8","DSC3","P3H2","TPPP3","SFTPD","IL13RA2","CLEC14A","SPOCK2","CLDN5","PLTP","HMGCS1","ACADL","LIFR","IGFBP4","SERPIND1","FZD5","TMPRSS4","GOLM1","ANXA3","ST6GALNAC1","PTGFRN","HSD17B6","PAPSS2","KRT6A","TNNC1","CCDC80","EMP2"]

# site_gse = ["GSE52463","GSE63073","GSE1122","GSE72073","GSE24206","GSE27597","GSE29133","GSE31934","GSE37768"]