
x<-read.csv("interim_growth_analysis_inputs.csv")
sc<-read.csv("agg_caaspp.csv")

x$grade<-as.numeric(x$grade_level_code)
x$year<-x$spring_year
x$id<-as.character(x$state_student_id)
sc$grade<-as.numeric(sc$grade_assessed)
sc$year<-sc$spring_year
sc$id<-as.character(sc$state_student_id)

##check for individual-level duplicates
x$tmp.id<-paste(x$id,x$year,x$grade)
tab<-table(x$tmp.id)
table(tab)
x$tmp.id<-NULL

##make separate math and reading datasets
keep<-c("district_code", "school_code", "school_name",  
        "enrollment_start_date", "enrollment_exit_date", 
        "enrollment_status_code", "census", "consecutive_enr_days", 
        "race_eth_rollup", "free_or_reduced", 
        "homeless", "foster", "migrant", "tribal_foster_youth", "direct_certification", 
        "sped_flag", "disability_1_code", "disability_2_code", "elas_status_code", 
        "elas_start_date", "elpac_level", "grade", "school_level", "socioecon_disadvantaged", 
        "race_ai", "race_as", "race_aa", "race_fi", "race_hi", "race_mr", 
        "race_pi", "race_na", "race_wh", "cds_code", "disability_type", 
        "ell_level", "core_elp", "core_elrfep", "reclass_year", "core_el", 
        "core_entity", "sub_entity", "first_test_date",
        "year", "id","grade")


for (nm in c("_ela","_math")) {
    print(nm)
    ##Additionally, all models control for disability, English language learner, economic disadvantage, foster care, and homelessness statuses at the student level. 
    i<-grep(nm,names(x))
    df<-x[,c(keep,names(x)[i])]
    names(df)<-sub(nm,'',names(df))
    schools<-df[,c("id","school_code","year","grade")] #save for computation of prior-year means
    ##mucking with grades for 11
    df<-df[df$grade %in% c(3:8,11) & !is.na(df$grade),]
    df$pretest.year<-ifelse(df$grade==11,df$year-3,df$year-1)
    ##lag score and prior year mean
    xx<-c("id","year","grade")
    xx<-c(xx,ifelse(nm=="_ela","ela_scale_score","math_scale_score"))
    x0<-sc[,xx]
    names(x0)[4]<-'scale_score'
    names(x0)[3]<-'pretest.grade'
    names(x0)[2]<-'pretest.year'
    names(x0)[3:4]<-paste("lag_",names(x0)[3:4],sep='')
    ##alt lag
    if (nm=="_ela") altlag<-sc$math_scale_score else altlag<-sc$ela_scale_score
    x0$alt_lag_scale_score<-altlag
    ##
    x0<-x0[!is.na(x0$lag_scale_score),]
    df<-merge(df,x0,all.x=TRUE) #this will break grade 11
    ##prior year mean
    xx<-c("id","year")
    xx<-c(xx,ifelse(nm=="_ela","ela_scale_score","math_scale_score"))
    x0<-sc[,xx]
    names(x0)[3]<-'scale_score'
    names(x0)[2]<-'pretest.year'
    x0<-x0[!is.na(x0$scale_score),]
    sch<-merge(schools,x0)
    sch$tmp.id<-paste(sch$year,sch$grade,sch$school_code)
    m1<-by(sch$scale_score,sch$tmp.id,mean,na.rm=TRUE)
    m2<-by(sch$scale_score,sch$tmp.id,function(x) length(x[!is.na(x)]))
    z<-data.frame(tmp.id=names(m1),lag_mean=unlist(m1))
    z2<-data.frame(tmp.id=names(m2),lag_mean_N=unlist(m2))
    z<-merge(z,z2)
    df$tmp.id<-paste(df$year,df$grade,df$school_code)
    df<-merge(df,z)
    df$tmp.id<-NULL
    ##
    df$subject<-nm
    save(df,file=paste(nm,".Rdata",sep=''))
}

    
