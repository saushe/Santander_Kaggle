load("Sample.R")

# 1 Basic Cleaning
# Get missing values for all the columns
count_missing<- function(x)
{
  y = length(which(is.na(x))==T)
  return(y)
}

# Replcae *blank* with NA
Data_Sampled[Data_Sampled==""]<- NA

missing_count<- summarise_all(Data_Sampled, count_missing)

# 1.1 Missing value treatment for columns with 2873 missin values
missing2873<- unique(Data_Sampled$ncodpers[is.na(Data_Sampled$age)==T])
Cleaned_data<- Data_Sampled[-which(Data_Sampled$ncodpers %in% missing2873),]
#OR
Cleaned_data<- filter(Data_Sampled, !(ncodpers %in% missing2873))

missing_count<- summarise_all(Cleaned_data, count_missing)

# 1.2 Remove renta, conyuemp, ult_fec_cli_1t
Cleaned_data<- select(Cleaned_data, -c(renta, conyuemp, ult_fec_cli_1t))

# 1.3 Treatmemt for rest of the data
# 1.3.1 For treating rest of the columns we need the row with which to replace the NA rows
# For this we create a flag at the maximum bought product

# 1.3.1.1 # Replace multiple '1s' of the product with one 1

Cleaned_data<- arrange(Cleaned_data, ncodpers, fecha_dato)
Cleaned_data<- group_by(Cleaned_data, ncodpers)

# Separate out product data and demograpics data
Demographic<- select(Cleaned_data, fecha_dato:segmento)
Prod_data<- select(Cleaned_data, c(ncodpers, fecha_dato, ind_ahor_fin_ult1:ind_recibo_ult1) )


# Just a check: how many customers have not bought any thing in the given period
Cust_ceck<- select(Cleaned_data, c(ncodpers, fecha_dato, ind_ahor_fin_ult1:ind_recibo_ult1) )
Cust_ceck<- ungroup(Cust_ceck)
summarise_all(Cust_ceck, class)
Cust_ceck<- mutate_at(Cust_ceck, vars(ends_with("ind")), as.numeric)
Cust_ceck1<- data.frame(Cust_ceck[1:2], apply(Cust_ceck[3:26],1, sum))
#OR Use something like the following
# companiesData$sums <- apply(companiesData[,c('revenue', 'profit')], 1, function(x) sum(x))
Cust_ceck1<- group_by(Cust_ceck1, ncodpers)
Cust_ceck1<- summarise(Cust_ceck1, x22 = sum(apply.Cust_ceck.3.26...1..sum.))
length(which(Cust_ceck1$x22==0)) #22397 who have not purchased anything at all in this time period
No_prod_Cust<- unique(filter(Cust_ceck1, x22 == 0 ))
No_prod_Cust<- as.vector(No_prod_Cust$ncodpers)
rm(Cust_ceck,Cust_ceck1)

# Create a duplicate for customesr column
rm(Cleaned_data)
Prod_data$cust_dup<- duplicated(Prod_data$ncodpers)
Prod_data<- group_by(Prod_data, ncodpers)

Prod_data<- mutate(Prod_data, ind_ahor_fin_ult2=ifelse(cust_dup==FALSE, ind_ahor_fin_ult1, diff(ind_ahor_fin_ult1)), 
                   ind_aval_fin_ult2=ifelse(cust_dup==FALSE, ind_aval_fin_ult1, diff(ind_aval_fin_ult1)), 
                   ind_cco_fin_ult2=ifelse(cust_dup==FALSE, ind_cco_fin_ult1, diff(ind_cco_fin_ult1)), 
                   ind_cder_fin_ult2=ifelse(cust_dup==FALSE, ind_cder_fin_ult1, diff(ind_cder_fin_ult1)), 
                   ind_cno_fin_ult2=ifelse(cust_dup==FALSE, ind_cno_fin_ult1, diff(ind_cno_fin_ult1)), 
                   ind_ctju_fin_ult2=ifelse(cust_dup==FALSE, ind_ctju_fin_ult1, diff(ind_ctju_fin_ult1)), 
                   ind_ctma_fin_ult2=ifelse(cust_dup==FALSE, ind_ctma_fin_ult1, diff(ind_ctma_fin_ult1)), 
                   ind_ctop_fin_ult2=ifelse(cust_dup==FALSE, ind_ctop_fin_ult1, diff(ind_ctop_fin_ult1)), 
                   ind_ctpp_fin_ult2=ifelse(cust_dup==FALSE, ind_ctpp_fin_ult1, diff(ind_ctpp_fin_ult1)), 
                   ind_deco_fin_ult2=ifelse(cust_dup==FALSE, ind_deco_fin_ult1, diff(ind_deco_fin_ult1)), 
                   ind_deme_fin_ult2=ifelse(cust_dup==FALSE, ind_deme_fin_ult1, diff(ind_deme_fin_ult1)), 
                   ind_dela_fin_ult2=ifelse(cust_dup==FALSE, ind_dela_fin_ult1, diff(ind_dela_fin_ult1)), 
                   ind_ecue_fin_ult2=ifelse(cust_dup==FALSE, ind_ecue_fin_ult1, diff(ind_ecue_fin_ult1)), 
                   ind_fond_fin_ult2=ifelse(cust_dup==FALSE, ind_fond_fin_ult1, diff(ind_fond_fin_ult1)), 
                   ind_hip_fin_ult2=ifelse(cust_dup==FALSE, ind_hip_fin_ult1, diff(ind_hip_fin_ult1)), 
                   ind_plan_fin_ult2=ifelse(cust_dup==FALSE, ind_plan_fin_ult1, diff(ind_plan_fin_ult1)), 
                   ind_pres_fin_ult2=ifelse(cust_dup==FALSE, ind_pres_fin_ult1, diff(ind_pres_fin_ult1)), 
                   ind_reca_fin_ult2=ifelse(cust_dup==FALSE, ind_reca_fin_ult1, diff(ind_reca_fin_ult1)), 
                   ind_tjcr_fin_ult2=ifelse(cust_dup==FALSE, ind_tjcr_fin_ult1, diff(ind_tjcr_fin_ult1)), 
                   ind_valo_fin_ult2=ifelse(cust_dup==FALSE, ind_valo_fin_ult1, diff(ind_valo_fin_ult1)), 
                   ind_viv_fin_ult2=ifelse(cust_dup==FALSE, ind_viv_fin_ult1, diff(ind_viv_fin_ult1)), 
                   ind_nomina_ult2=ifelse(cust_dup==FALSE, ind_nomina_ult1, diff(ind_nomina_ult1)), 
                   ind_nom_pens_ult2=ifelse(cust_dup==FALSE, ind_nom_pens_ult1, diff(ind_nom_pens_ult1)), 
                   ind_recibo_ult2=ifelse(cust_dup==FALSE, ind_recibo_ult1, diff(ind_recibo_ult1)))


Prod_data<- Prod_data[,c(1:2,27:51)]
Prod_data<- arrange(Prod_data, desc(ncodpers, fecha_dato))
# remove Negatives
Prod_data2<- Prod_data
Prod_data2[Prod_data2==-1]<- 0

# Create Distinct Column
Prod_data2<- group_by(Prod_data2, ncodpers)
Disc_Summary<- summarise(Prod_data2, ind_ahor_fin_ult2_disc = sum(ind_ahor_fin_ult2),
                      ind_aval_fin_ult2_disc = sum(ind_aval_fin_ult2),
                      ind_cco_fin_ult2_disc = sum(ind_cco_fin_ult2),
                      ind_cder_fin_ult2_disc = sum(ind_cder_fin_ult2),
                      ind_cno_fin_ult2_disc = sum(ind_cno_fin_ult2),
                      ind_ctju_fin_ult2_disc = sum(ind_ctju_fin_ult2),
                      ind_ctma_fin_ult2_disc = sum(ind_ctma_fin_ult2),
                      ind_ctop_fin_ult2_disc = sum(ind_ctop_fin_ult2),
                      ind_ctpp_fin_ult2_disc = sum(ind_ctpp_fin_ult2),
                      ind_deco_fin_ult2_disc = sum(ind_deco_fin_ult2),
                      ind_deme_fin_ult2_disc = sum(ind_deme_fin_ult2),
                      ind_dela_fin_ult2_disc = sum(ind_dela_fin_ult2),
                      ind_ecue_fin_ult2_disc = sum(ind_ecue_fin_ult2),
                      ind_fond_fin_ult2_disc = sum(ind_fond_fin_ult2),
                      ind_hip_fin_ult2_disc = sum(ind_hip_fin_ult2),
                      ind_plan_fin_ult2_disc = sum(ind_plan_fin_ult2),
                      ind_pres_fin_ult2_disc = sum(ind_pres_fin_ult2),
                      ind_reca_fin_ult2_disc = sum(ind_reca_fin_ult2),
                      ind_tjcr_fin_ult2_disc = sum(ind_tjcr_fin_ult2),
                      ind_valo_fin_ult2_disc = sum(ind_valo_fin_ult2),
                      ind_viv_fin_ult2_disc = sum(ind_viv_fin_ult2),
                      ind_nomina_ult2_disc = sum(ind_nomina_ult2),
                      ind_nom_pens_ult2_disc = sum(ind_nom_pens_ult2),
                      ind_recibo_ult2_disc = sum(ind_recibo_ult2))

# Remove the customers who had not made any purchase in the given time period
Prod_data2<- filter(Prod_data2, !(ncodpers %in% No_prod_Cust))


# Leave only one 1 in the column
Prod_data2<- ungroup(Prod_data2)
Prod_data2 <- Prod_data2[with(Prod_data2, order(Prod_data2$ncodpers, Prod_data2$fecha_dato, decreasing=TRUE)),]
Prod_data2<- ungroup(Prod_data2)
summarise_all(Prod_data2, class)
Prod_data2<- mutate_at(Prod_data2 , vars(ends_with("2")), as.numeric)

Prod_data2<- group_by(Prod_data2, ncodpers)
Prod_data2<- mutate(Prod_data2, Fin_ind_ahor_fin_ult2=ifelse(ind_ahor_fin_ult2==1,as.numeric(!duplicated(ind_ahor_fin_ult2)),ind_ahor_fin_ult2),
                    Fin_ind_aval_fin_ult2=ifelse(ind_aval_fin_ult2==1,as.numeric(!duplicated(ind_aval_fin_ult2)),ind_aval_fin_ult2),
                    Fin_ind_cco_fin_ult2=ifelse(ind_cco_fin_ult2==1,as.numeric(!duplicated(ind_cco_fin_ult2)),ind_cco_fin_ult2),
                    Fin_ind_cder_fin_ult2=ifelse(ind_cder_fin_ult2==1,as.numeric(!duplicated(ind_cder_fin_ult2)),ind_cder_fin_ult2),
                    Fin_ind_cno_fin_ult2=ifelse(ind_cno_fin_ult2==1,as.numeric(!duplicated(ind_cno_fin_ult2)),ind_cno_fin_ult2),
                    Fin_ind_ctju_fin_ult2=ifelse(ind_ctju_fin_ult2==1,as.numeric(!duplicated(ind_ctju_fin_ult2)),ind_ctju_fin_ult2),
                    Fin_ind_ctma_fin_ult2=ifelse(ind_ctma_fin_ult2==1,as.numeric(!duplicated(ind_ctma_fin_ult2)),ind_ctma_fin_ult2),
                    Fin_ind_ctop_fin_ult2=ifelse(ind_ctop_fin_ult2==1,as.numeric(!duplicated(ind_ctop_fin_ult2)),ind_ctop_fin_ult2),
                    Fin_ind_ctpp_fin_ult2=ifelse(ind_ctpp_fin_ult2==1,as.numeric(!duplicated(ind_ctpp_fin_ult2)),ind_ctpp_fin_ult2),
                    Fin_ind_deco_fin_ult2=ifelse(ind_deco_fin_ult2==1,as.numeric(!duplicated(ind_deco_fin_ult2)),ind_deco_fin_ult2),
                    Fin_ind_deme_fin_ult2=ifelse(ind_deme_fin_ult2==1,as.numeric(!duplicated(ind_deme_fin_ult2)),ind_deme_fin_ult2),
                    Fin_ind_dela_fin_ult2=ifelse(ind_dela_fin_ult2==1,as.numeric(!duplicated(ind_dela_fin_ult2)),ind_dela_fin_ult2),
                    Fin_ind_ecue_fin_ult2=ifelse(ind_ecue_fin_ult2==1,as.numeric(!duplicated(ind_ecue_fin_ult2)),ind_ecue_fin_ult2),
                    Fin_ind_fond_fin_ult2=ifelse(ind_fond_fin_ult2==1,as.numeric(!duplicated(ind_fond_fin_ult2)),ind_fond_fin_ult2),
                    Fin_ind_hip_fin_ult2=ifelse(ind_hip_fin_ult2==1,as.numeric(!duplicated(ind_hip_fin_ult2)),ind_hip_fin_ult2),
                    Fin_ind_plan_fin_ult2=ifelse(ind_plan_fin_ult2==1,as.numeric(!duplicated(ind_plan_fin_ult2)),ind_plan_fin_ult2),
                    Fin_ind_pres_fin_ult2=ifelse(ind_pres_fin_ult2==1,as.numeric(!duplicated(ind_pres_fin_ult2)),ind_pres_fin_ult2),
                    Fin_ind_reca_fin_ult2=ifelse(ind_reca_fin_ult2==1,as.numeric(!duplicated(ind_reca_fin_ult2)),ind_reca_fin_ult2),
                    Fin_ind_tjcr_fin_ult2=ifelse(ind_tjcr_fin_ult2==1,as.numeric(!duplicated(ind_tjcr_fin_ult2)),ind_tjcr_fin_ult2),
                    Fin_ind_valo_fin_ult2=ifelse(ind_valo_fin_ult2==1,as.numeric(!duplicated(ind_valo_fin_ult2)),ind_valo_fin_ult2),
                    Fin_ind_viv_fin_ult2=ifelse(ind_viv_fin_ult2==1,as.numeric(!duplicated(ind_viv_fin_ult2)),ind_viv_fin_ult2),
                    Fin_ind_nomina_ult2=ifelse(ind_nomina_ult2==1,as.numeric(!duplicated(ind_nomina_ult2)),ind_nomina_ult2),
                    Fin_ind_nom_pens_ult2=ifelse(ind_nom_pens_ult2==1,as.numeric(!duplicated(ind_nom_pens_ult2)),ind_nom_pens_ult2),
                    Fin_ind_recibo_ult2=ifelse(ind_recibo_ult2==1,as.numeric(!duplicated(ind_recibo_ult2)),ind_recibo_ult2))


Prod_data2<- Prod_data2[,c(1,2,28:51)]

# Create Max date Flag for each customer
Prod_data2$prod_sum<- apply(Prod_data2[3:26], 1, sum)

Prod_data2<- group_by(Prod_data2, ncodpers)
Prod_data2<- mutate(Prod_data2, max_date = max(fecha_dato[which(prod_sum != 0)]))

rm(Prod_data)

# remove all the NAs in the Data
Final_Prod<- filter(Prod_data2, is.na(max_date)==F)

Max_Date<- group_by(Fin_Prod, ncodpers) %>% summarise(max_date1 = max(max_date))
                                                            
# Now Join Max_Date with demographic Data and Indentify the target product for each customer

# Identifying the target product

Target_prod<-Final_Prod
Target_prod$max_flag<- ifelse(Target_prod$max_date == Target_prod$fecha_dato, 1, 0)
Target_prod<- filter(Target_prod, max_flag == 1)
Target_prod<- Target_prod[, c(1, 3:26)]

Target_prod<- ungroup(Target_prod)
Target_prod1<- melt(Target_prod, id = "ncodpers")
Target_prod1<- arrange(Target_prod1, ncodpers)
 
Target_prod1<- filter(Target_prod1, value>0)
Target_prod1<- arrange(Target_prod1, variable)

Prod_dist<- group_by(Target_prod1, variable)%>%summarise(length(variable))%>% arrange(`length(variable)`)

fun<- function() data.frame(a = c(1,2))
y<- replicate(24,fun())
y[[1]]<- data.frame(a = c(1,2), b = c(3,2))

product_names<- unique(Target_prod1$variable)
test<- merge(Max_Date, Target_prod1, by.x = "ncodpers", by.y = "ncodpers", all.x = T) 

# Round 2
# Removing NA from the demographic data
Demographic<- ungroup(Demographic)
# For indrel_1mes
Cus_For_indrel_1mes<- Demographic[,c(2,11)]
Cus_For_indrel_1mes<- unique(Cus_For_indrel_1mes)
length(unique(Cus_For_indrel_1mes$ncodpers))
Cus_For_indrel_1mes$Na_Flag<- ifelse(is.na(Cus_For_indrel_1mes$indrel_1mes)==T, 1, 0)
CusWithNA_indrel_1mes<- Cus_For_indrel_1mes[which(Cus_For_indrel_1mes$Na_Flag==1),]
CusWithNA_indrel_1mes<- unique(CusWithNA_indrel_1mes)
Cus_For_indrel_1mes<- Cus_For_indrel_1mes[-which(Cus_For_indrel_1mes$Na_Flag==1),]
a<- unique(Cus_For_indrel_1mes$ncodpers)
b<- unique(CusWithNA_indrel_1mes$ncodpers)
z<- intersect(a,b)
CusWithNA_indrel_1mes1<- merge(CusWithNA_indrel_1mes, Cus_For_indrel_1mes, by.x = "ncodpers", by.y = "ncodpers", all.x = T)
CusWithNA_indrel_1mes1$Dup<- duplicated(CusWithNA_indrel_1mes1$ncodpers)
CusWithNA_indrel_1mes1<- CusWithNA_indrel_1mes1[-(which(CusWithNA_indrel_1mes1$Dup==TRUE)),]
CusWithNA_indrel_1mes1<- CusWithNA_indrel_1mes1[,c(1,4)]  
rm(Cus_For_indrel_1mes)
rm(CusWithNA_indrel_1mes)

#For Segmentaion

Cus_For_segmento<- Demographic[,c(2,21)]
Cus_For_segmento<- unique(Cus_For_segmento)
length(unique(Cus_For_segmento$ncodpers))
Cus_For_segmento$Na_Flag<- ifelse(is.na(Cus_For_segmento$segmento)==T, 1, 0)
CusWithNA_segmento<- Cus_For_segmento[which(Cus_For_segmento$Na_Flag==1),]
CusWithNA_segmento<- unique(CusWithNA_segmento)
Cus_For_segmento<- Cus_For_segmento[-which(Cus_For_segmento$Na_Flag==1),]
a<- unique(Cus_For_segmento$ncodpers)
b<- unique(CusWithNA_segmento$ncodpers)
z<- intersect(a,b)
CusWithNA_segmento1<- merge(CusWithNA_segmento, Cus_For_segmento, by.x = "ncodpers", by.y = "ncodpers", all.x = T)
CusWithNA_segmento1$Dup<- duplicated(CusWithNA_segmento1$ncodpers)
CusWithNA_segmento1<- CusWithNA_segmento1[-(which(CusWithNA_segmento1$Dup==TRUE)),]
CusWithNA_segmento1<- CusWithNA_segmento1[,c(1,4)]  
rm(Cus_For_segmento)
rm(CusWithNA_segmento)


#For tiprel_1mes

Cus_For_tiprel_1mes<- Demographic[,c(2,12)]
Cus_For_tiprel_1mes<- unique(Cus_For_tiprel_1mes)
length(unique(Cus_For_tiprel_1mes$ncodpers))
Cus_For_tiprel_1mes$Na_Flag<- ifelse(is.na(Cus_For_tiprel_1mes$tiprel_1mes)==T, 1, 0)
CusWithNA_tiprel_1mes<- Cus_For_tiprel_1mes[which(Cus_For_tiprel_1mes$Na_Flag==1),]
CusWithNA_tiprel_1mes<- unique(CusWithNA_tiprel_1mes)
Cus_For_tiprel_1mes<- Cus_For_tiprel_1mes[-which(Cus_For_tiprel_1mes$Na_Flag==1),]
a<- unique(Cus_For_tiprel_1mes$ncodpers)
b<- unique(CusWithNA_tiprel_1mes$ncodpers)
z<- intersect(a,b)
CusWithNA_tiprel_1mes1<- merge(CusWithNA_tiprel_1mes, Cus_For_tiprel_1mes, by.x = "ncodpers", by.y = "ncodpers", all.x = T)
CusWithNA_tiprel_1mes1$Dup<- duplicated(CusWithNA_tiprel_1mes1$ncodpers)
CusWithNA_tiprel_1mes1<- CusWithNA_tiprel_1mes1[-(which(CusWithNA_tiprel_1mes1$Dup==TRUE)),]
CusWithNA_tiprel_1mes1<- CusWithNA_tiprel_1mes1[,c(1,4)]  
rm(Cus_For_tiprel_1mes)
rm(CusWithNA_tiprel_1mes)

#For canal_entrada

Cus_For_canal_entrada<- Demographic[,c(2,15)]
Cus_For_canal_entrada<- unique(Cus_For_canal_entrada)
length(unique(Cus_For_canal_entrada$ncodpers))
Cus_For_canal_entrada$Na_Flag<- ifelse(is.na(Cus_For_canal_entrada$canal_entrada)==T, 1, 0)
CusWithNA_canal_entrada<- Cus_For_canal_entrada[which(Cus_For_canal_entrada$Na_Flag==1),]
CusWithNA_canal_entrada<- unique(CusWithNA_canal_entrada)
Cus_For_canal_entrada<- Cus_For_canal_entrada[-which(Cus_For_canal_entrada$Na_Flag==1),]
a<- unique(Cus_For_canal_entrada$ncodpers)
b<- unique(CusWithNA_canal_entrada$ncodpers)
z<- intersect(a,b)
CusWithNA_canal_entrada1<- merge(CusWithNA_canal_entrada, Cus_For_canal_entrada, by.x = "ncodpers", by.y = "ncodpers", all.x = T)
CusWithNA_canal_entrada1$Dup<- duplicated(CusWithNA_canal_entrada1$ncodpers)
CusWithNA_canal_entrada1<- CusWithNA_canal_entrada1[-(which(CusWithNA_canal_entrada1$Dup==TRUE)),]
CusWithNA_canal_entrada1<- CusWithNA_canal_entrada1[,c(1,4)]  
rm(Cus_For_canal_entrada)
rm(CusWithNA_canal_entrada)


# Merge New Columns with the Orignal Data
Demographic<- merge(Demographic, CusWithNA_canal_entrada1, by.x = "ncodpers", by.y = "ncodpers", all.x = T)
Demographic<- merge(Demographic, CusWithNA_segmento1, by.x = "ncodpers", by.y = "ncodpers", all.x = T)
Demographic<- merge(Demographic, CusWithNA_tiprel_1mes1, by.x = "ncodpers", by.y = "ncodpers", all.x = T)
Demographic<- merge(Demographic, CusWithNA_indrel_1mes1, by.x = "ncodpers", by.y = "ncodpers", all.x = T)

rm(CusWithNA_canal_entrada1, CusWithNA_tiprel_1mes1, CusWithNA_indrel_1mes1, CusWithNA_segmento1)

# Create new columns
Demographic$canal_entrada_New<- ifelse((is.na(Demographic$canal_entrada)==F), Demographic$canal_entrada, Demographic$canal_entrada.y)
Demographic$segmento_New<- ifelse((is.na(Demographic$segmento)==F), Demographic$segmento, Demographic$segmento.y)
Demographic$tiprel_1mes_New<- ifelse((is.na(Demographic$tiprel_1mes)==F), Demographic$tiprel_1mes, Demographic$tiprel_1mes.y)
Demographic$indrel_1mes_New<- ifelse((is.na(Demographic$indrel_1mes)==F), Demographic$indrel_1mes, Demographic$indrel_1mes.y)

# Check for Na

# Get the users with NAs
JC<- unique(Demographic$ncodpers[is.na(Demographic$canal_entrada_New)==T ])
SG<- unique(Demographic$ncodpers[is.na(Demographic$segmento_New)==T ])
CR<- unique(Demographic$ncodpers[is.na(Demographic$tiprel_1mes_New)==T ])
CT<- unique(Demographic$ncodpers[is.na(Demographic$indrel_1mes_New)==T ])

Master<- c(JC, SG, CR, CT)
Master<- unique(Master)


# Get final Data without renta

Demographic<- Demographic[-which(Demographic$ncodpers %in% Master),]
Demographic<- Demographic[,-c(11,12,15,21:25)]


Na_Cnt_Demographic<- as.data.frame(apply(Demographic, 2, function(x){ length(which(is.na(x)==T))   }))

# Treartment for cod_prov and nomprov
cust_list_NA_cod_prov<- unique(Demographic$ncodpers[is.na(Demographic$cod_prov)==T])

cust_list_NA_nomprov<- unique(Demographic$ncodpers[is.na(Demographic$nomprov)==T])

#These are the same people
#decided to remove the customers
Final_Demographic<- Demographic[-c(which(Demographic$ncodpers %in% cust_list_NA_nomprov)),]

rm(Demographic)
Na_Cnt_Demographic<- as.data.frame(apply(Final_Demographic, 2, function(x){ length(which(is.na(x)==T))   }))

# Keep only that row where Product has been purchased
Final_Demographic<- merge(Final_Demographic, Max_Date, by.x = "ncodpers", by.y = "ncodpers", all.x= T)
Final_Demographic$Date_flag<- ifelse(Final_Demographic$fecha_dato==Final_Demographic$max_date1,1,0)
Final_Demographic<- filter(Final_Demographic, Date_flag==1)

# Now Supress Prodcut Data

Final_Prod<- group_by(Final_Prod, ncodpers)
Final_Prod_Summed<- summarise(Final_Prod, ind_ahor_fin_ult1 = sum ( Fin_ind_ahor_fin_ult2) ,
                       ind_aval_fin_ult1 = sum ( Fin_ind_aval_fin_ult2) ,
                       ind_cco_fin_ult1 = sum ( Fin_ind_cco_fin_ult2) ,
                       ind_cder_fin_ult1 = sum ( Fin_ind_cder_fin_ult2) ,
                       ind_cno_fin_ult1 = sum ( Fin_ind_cno_fin_ult2) ,
                       ind_ctju_fin_ult1 = sum ( Fin_ind_ctju_fin_ult2) ,
                       ind_ctma_fin_ult1 = sum ( Fin_ind_ctma_fin_ult2) ,
                       ind_ctop_fin_ult1 = sum ( Fin_ind_ctop_fin_ult2) ,
                       ind_ctpp_fin_ult1 = sum ( Fin_ind_ctpp_fin_ult2) ,
                       ind_deco_fin_ult1 = sum ( Fin_ind_deco_fin_ult2) ,
                       ind_deme_fin_ult1 = sum ( Fin_ind_deme_fin_ult2) ,
                       ind_dela_fin_ult1 = sum ( Fin_ind_dela_fin_ult2) ,
                       ind_ecue_fin_ult1 = sum ( Fin_ind_ecue_fin_ult2) ,
                       ind_fond_fin_ult1 = sum ( Fin_ind_fond_fin_ult2) ,
                       ind_hip_fin_ult1 = sum ( Fin_ind_hip_fin_ult2) ,
                       ind_plan_fin_ult1 = sum ( Fin_ind_plan_fin_ult2) ,
                       ind_pres_fin_ult1 = sum ( Fin_ind_pres_fin_ult2) ,
                       ind_reca_fin_ult1 = sum ( Fin_ind_reca_fin_ult2) ,
                       ind_tjcr_fin_ult1 = sum ( Fin_ind_tjcr_fin_ult2) ,
                       ind_valo_fin_ult1 = sum ( Fin_ind_valo_fin_ult2) ,
                       ind_viv_fin_ult1 = sum ( Fin_ind_viv_fin_ult2) ,
                       ind_nomina_ult1 = sum ( Fin_ind_nomina_ult2) ,
                       ind_nom_pens_ult1 = sum ( Fin_ind_nom_pens_ult2) ,
                       ind_recibo_ult1 = sum ( Fin_ind_recibo_ult2))
rm(Final_Prod)

# Merge Product and demographic data
Final_Demographic<- Final_Demographic[,-c(22,23)]
Final_Data<- merge(Final_Demographic, Final_Prod_Summed, by.x= "ncodpers", by.y = "ncodpers", all.x= T)

Final_Data<- filter(Final_Data, !is.na(Final_Data$ind_cder_fin_ult1)==T)

# Merge with Target Product

b<- Final_Data$ncodpers
Target_prod1<- filter(Target_prod1, ncodpers %in% b)

Final_Data<- merge(Target_prod1, Final_Data, by.x= "ncodpers", by.y = "ncodpers", all.x= T )
Final_Data<- select(Final_Data, -(value))
Final_Data<- merge(Final_Data, Disc_Summary,by.x= "ncodpers", by.y = "ncodpers", all.x= T  )

Final_Data$variable<- as.character(Final_Data$variable)


# Create 24 data sets and also merge discontinued information
# Create the column names to be pulled while creating 24 data sets
Prod_names<- as.character(colnames(Final_Data[23:46]))
Fin_Prod_names<- gsub("1", "2",Prod_names)
Fin_Prod_names<- paste0("Fin_",Fin_Prod_names )
Disc_prod_names<- gsub("1", "2",Prod_names)
Disc_prod_names<- paste0( Disc_prod_names, "_disc")


i = 1

for (i in 1:5) {
  
  k = which(colnames(Final_Data)==Prod_names[i])
  l = which(colnames(Final_Data)==Disc_prod_names[i])
  m = Fin_Prod_names[i]
  temp_data <-  Final_Data[,c(1:46, l) ]
  temp_data$variable<- ifelse(temp_data$variable == m, 1, 0)
  temp_data<- arrange(temp_data, desc(variable))
  temp_data$dup_cust<- duplicated(temp_data$ncodpers)
  temp_data<- filter(temp_data, dup_cust==F)
  temp_data<- temp_data[,-c(3,48)]
  assign(paste0("DF_", Prod_names[i]), temp_data)
  
}




# now I want a new data with 25 rare and 25 common
newData<- strata(data, stratanames =  "Species", size = c(25,25))
table(newData$Species)

# common   rare 
#   25     25 
# Dataframe size = 50

# YOU CAN ALSO CREATE STRATIFIED SAMPLE BASED ON TWO CLASS VARIABLES
 data=rbind(matrix(rep("nc",165),165,1,byrow=TRUE),matrix(rep("sc",70),70,1,byrow=TRUE))
 data=cbind.data.frame(data,c(rep(1,100), rep(2,50), rep(3,15), rep(1,30),rep(2,40)),
                        +                       1000*runif(235))
 names(data)=c("state","region","income")
 s=strata(data,c("state", "region"),size=c(10,5,10,4,6), method="srswor")
# in this you provide size for each combination of the two variables


# 3 for sampling a test dats set maintaining the popultion distribution, use createDatapattern
 x<- createDataPartition(data$state, p= .8, list = F)
 x<- as.vector(x)
 newData<- data[x,]
# this will take 80% of the population to create a sample


