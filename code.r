#Year 2 Sem 1
#YEE ZHI YING 
#TP055495
#--------------------------------------------------------------------------------#

#1.0 Import Dataset
library(data.table)
ori_dataset<-fread(input="C:\\Users\\yeezh_000\\Documents\\employee_attrition.csv", sep="," , header = TRUE)
print(as_tibble(ori_dataset), n =10)

#Loading Packages
library(dplyr)
library(tibble)
library(ggplot2)
library(forcats)
library(hrbrthemes)

#--------------------------------------------------------------------------------#

#2.0 Data Exploration
#Reference: https://www.listendata.com/2014/06/data-exploration-using-r.html

#2.1 Lists All Column Names
names(ori_dataset)

#2.2 List Sample Dataset
#Calculate number of rows in a dataset
nrow(ori_dataset)
#Calculate number of columns in a dataset
ncol(ori_dataset)
#First n rows of dataset, Last 6 rows of dataset
head(ori_dataset, n=10)
tail(ori_dataset, n=10)

#2.3 Data structure of dataset
str(ori_dataset)

#2.4 Missing Values in Dataset 
colSums(is.na(ori_dataset))

#2.5 Summary Statistics of Numeric Attributes 
summary(ori_dataset$EmployeeID)
summary(ori_dataset$age)
summary(ori_dataset$length_of_service)
summary(ori_dataset$store_name)
summary(ori_dataset$STATUS_YEAR)

#Installing packages for Additional Themes, Theme Components and Utilities for 'ggplot2'
#https://cran.r-project.org/web/packages/hrbrthemes/index.html
install.packages("hrbrthemes")
library(hrbrthemes)

#2.5.1 Histogram for EmployeeID
ori_dataset %>%
  ggplot( aes(x=EmployeeID)) +
  geom_histogram(breaks=seq(1000,8500 , by=500), col="#e9ecef",   aes(fill=..count..)) +
  scale_fill_gradient("Count", low="#30E3CA", high="#193498")+
  labs(title="Histogram for EmployeeID", x="EmployeeID", y="Count")+
  theme_ipsum() 

#2.5.2
#Histogram for Terminated (Age)
ori_dataset %>%
  filter(STATUS == "TERMINATED") %>%
  ggplot( aes(x=age)) +
  geom_histogram(breaks=seq(0,70 , by=5), col="#e9ecef",   aes(fill=..count..)) +
  scale_fill_gradient("Count", low="#30E3CA", high="#193498")+
  facet_wrap(~gender_short)+
  labs(title="Histogram for Age of Terminated Employee", x="Age", y="Count")+
  theme_ipsum() 

#Histogram for Active (Age)
ori_dataset %>%
  filter(STATUS == "ACTIVE") %>%
  ggplot( aes(x=age)) +
  geom_histogram(breaks=seq(10,70 , by=5), col="#e9ecef",   aes(fill=..count..)) +
  scale_fill_gradient("Count", low="#30E3CA", high="#193498")+
  facet_wrap(~gender_short)+
  labs(title="Histogram for Age of Active Employee", x="Age", y="Count")+
  theme_ipsum() 
 

#2.5.3 Histogram for Length of Service
ori_dataset %>%
    ggplot( aes(x=length_of_service)) +
    geom_histogram(breaks=seq(0,30 , by=2), col="#e9ecef",   aes(fill=..count..)) +
    scale_fill_gradient("Count", low="#30E3CA", high="#193498")+
    labs(title="Histogram for ", x="Length of Service (Year)", y="Count")+
    theme_ipsum() 

#2.5.4 Histogram for Store Name
ori_dataset %>%
  ggplot( aes(x=store_name)) +
  geom_histogram(breaks=seq(0,50 , by=2), col="#e9ecef",   aes(fill=..count..)) +
  scale_fill_gradient("Count", low="#30E3CA", high="#193498")+
  labs(title="Histogram for Store Number", x="Store Number", y="Count")+
  theme_ipsum() 

#2.5.5
#Line chart for Terminated (Status Year)
termStatYear<- ori_dataset%>% 
  filter(STATUS == "TERMINATED")%>%
  group_by(STATUS_YEAR) %>%
  summarise(count = n_distinct(EmployeeID, na.rm = TRUE)) %>%
  ggplot(aes(x=STATUS_YEAR, y=count)) +
  geom_line(size=0.8, colour="#193498")+
  geom_point(size=3, colour="#28CC9E")+
  xlim(2005,2016)+
  labs(title="Terminated Employee's Year of Status", x="Year of Status", y="Count")+
  theme_ipsum() 

#Line chart for Active (Status Year)
activeStatYear<- ori_dataset%>% 
  filter(STATUS == "ACTIVE")%>%
  group_by(STATUS_YEAR) %>%
  summarise(count = n_distinct(EmployeeID, na.rm = TRUE)) %>%
  ggplot(aes(x=STATUS_YEAR, y=count)) +
  geom_line(size=0.8, colour="#FFC947")+
  geom_point(size=3, colour="#F8485E")+
  xlim(2005,2016)+
  labs(title="Active Employee's Year of Status", x="Year of Status", y="Count")+
  theme_ipsum() 

#2.6 Distribution of Character Attributes
#2.6.1
#Bar chart for City Name
city_name.freq <- ori_dataset %>% 
  count(city_name)
view(city_name.freq)
city_name.freq%>%
  mutate(city_name = fct_reorder(city_name, n))%>%
  ggplot(aes(x = city_name, y= n)) +
  geom_bar(stat = "identity", colour="#FFFFFF", fill= "#28CC9E")+
  coord_flip()+
  geom_text(aes(label = n), size= 3, hjust=-0.15) + 
  labs(title = "Bar Chart of City Name", x = "City Name", y = "Count") + 
  theme_bw()

#2.6.2 Bar chart for Department Name
deptname.freq<- ori_dataset %>% 
  count(department_name) %>%
  mutate(department_name = fct_reorder(department_name, n))%>%
  ggplot(aes(x = department_name, y= n)) +
  geom_bar(stat = "identity", colour="#FFFFFF", fill= "#28CC9E")+
  coord_flip()+
  geom_text(aes(label = n), size= 3, hjust=-0.15) + 
  labs(title = "Bar Chart of Department Name", x = "Department Name", y = "Count") + 
  theme_bw()

#2.6.3 Bar chart for Job Title
jobtitle.freq <- ori_dataset %>% 
  count(job_title) %>%
  mutate(job_title = fct_reorder(job_title, n))%>%
  ggplot(aes(x = job_title, y= n)) +
  geom_bar(stat = "identity", colour="#FFFFFF", fill= "#28CC9E")+
  coord_flip()+
  geom_text(aes(label = n), size= 3, hjust=-0.15) + 
  labs(title = "Bar Chart of Job Title", x = "Job Title", y = "Count") + 
  theme_bw()

#2.6.4 Bar chart for Gender
gender.freqplot <- ori_dataset %>% 
  count(gender_short) %>%
  mutate(gender_short = fct_reorder(gender_short, n))%>%
  ggplot(aes(x = gender_short, y= n)) +
  geom_bar(stat = "identity", colour="#FFFFFF", fill= "#28CC9E", width=0.5)+
  geom_text(aes(label = n), size= 3, vjust=-0.5) + 
  labs(title = "Bar Chart of Gender", x = "Gender", y = "Count") + 
  theme_bw()

#2.6.5 Bar chart for Termination Reason
termreason.freqplot <- ori_dataset %>% 
  count(termreason_desc) %>%
  mutate(termreason_desc = fct_reorder(termreason_desc, n))%>%
  ggplot(aes(x = termreason_desc, y= n)) +
  geom_bar(stat = "identity", colour="#FFFFFF", fill= "#28CC9E", width=0.5)+
  geom_text(aes(label = n), size= 3, vjust=-0.5) + 
  labs(title = "Bar Chart of Reason of Termination", x = "Termination Reason", y = "Count") + 
  theme_bw()

#2.6.6 Bar chart for Termination Type
termtype.freqplot<- ori_dataset %>% 
  count(termtype_desc) %>%
  mutate(termtype_desc = fct_reorder(termtype_desc, n))%>%
  ggplot(aes(x = termtype_desc, y= n)) +
  geom_bar(stat = "identity", colour="#FFFFFF", fill= "#28CC9E", width=0.5)+
  geom_text(aes(label = n), size= 3, vjust=-0.5) + 
  labs(title = "Bar Chart of Type of Termination", x = "Type of Termination", y = "Count") + 
  theme_bw()

#2.6.7 Bar chart for Status
status.freqplot<- ori_dataset %>% 
  count(STATUS) %>%
  mutate(STATUS = fct_reorder(STATUS, n))%>%
  ggplot(aes(x = STATUS, y= n)) +
  geom_bar(stat = "identity", colour="#FFFFFF", fill= "#28CC9E", width=0.5)+
  geom_text(aes(label = n), size= 3, vjust=-0.5) + 
  labs(title = "Bar Chart of Employee Status", x = "Employee Status", y = "Count") + 
  theme_bw()

#2.6.8 Bar chart for Business Unit
businessUnit.freqplot<- ori_dataset %>% 
  count(BUSINESS_UNIT) %>%
  ggplot(aes(x = BUSINESS_UNIT, y= n)) +
  geom_bar(stat = "identity", colour="#FFFFFF", fill= "#28CC9E", width=0.5)+
  geom_text(aes(label = n), size= 3, vjust=-0.5) + 
  labs(title = "Bar Chart of Business Unit", x = "Business Unit", y = "Count") + 
  theme_bw()

#--------------------------------------------------------------------------------#

#3.0 Data Transformation
#3.1 Rename Columns
OriDataset.RenameCol = ori_dataset %>%
  rename(Employee_ID = EmployeeID,
         Record_Date = recorddate_key,
         Birthday = birthdate_key,
         Hire_Date = orighiredate_key,
         Termination_Date = terminationdate_key,
         Age = age,
         Length_of_Service = length_of_service,
         City_Name = city_name,
         Department_Name = department_name,
         Job_Title = job_title,
         Store_Name = store_name,
         Gender = gender_short,
         Gender_Desc = gender_full,
         Term_Reason = termreason_desc,
         Term_Type = termtype_desc,
         Status = STATUS,
         Status_Year = STATUS_YEAR,
         Business_Unit = BUSINESS_UNIT)
str(OriDataset.RenameCol)

#3.2 Date Formatting for all Date Attributes & add Year Month Columns
OriDataset.TransDate = OriDataset.RenameCol %>% 
  mutate(Record_Date =  as.Date(Record_Date, format="%m/%d/%Y"),  
         Birthday = as.Date(Birthday, format="%m/%d/%Y"),
         Hire_Date = as.Date(Hire_Date, format="%m/%d/%Y"),
         Termination_Date = as.Date(Termination_Date, format="%m/%d/%Y"))

OriDataset.TransDate = OriDataset.TransDate %>%
  add_column(Hire_Year = format(OriDataset.TransDate$Hire_Date, format="%Y"), 
             Hire_Month = format(OriDataset.TransDate$Hire_Date, format="%b"), .after = "Hire_Date")%>%
  add_column(Term_Year = ifelse(OriDataset.TransDate$Termination_Date == "1900-01-01", 
                                NA, format(OriDataset.TransDate$Termination_Date, format="%Y")),
             Term_Month = ifelse(OriDataset.TransDate$Termination_Date == "1900-01-01", 
                                 NA, format(OriDataset.TransDate$Termination_Date, format="%b")), 
             .after = "Termination_Date")

str(OriDataset.TransDate)
view(OriDataset.TransDate)


#3.3 Replacing values with NA and small case letter
OriDataset.RepValues <- OriDataset.TransDate %>% 
  mutate(Term_Reason = ifelse(Term_Reason=="Not Applicable", NA, Term_Reason),
         Term_Type = ifelse(Term_Type == "Not Applicable", NA, Term_Type),
         Status = ifelse(Status == "ACTIVE", "Active", "Terminated"),
         Business_Unit = ifelse(Business_Unit == "HEADOFFICE", "Head Office", "Stores"))
view(OriDataset.RepValues)


#3.4 Creating Age Group as new Column
OriDataset.AgeGroup <- OriDataset.RepValues %>%
 add_column(Age_Group = cut(OriDataset.RepValues$Age, breaks = c(14,24,34,54,Inf),
                          labels = c("15-24", "25-34", "35-54", "55-74" )),
      Age_Group_Desc = cut(OriDataset.RepValues$Age, breaks = c(14,24,34,54,Inf),
                           labels = c("Student", "Adult", "Upper-Middle", "Senior" )), .after = "Age")
str(OriDataset.AgeGroup)
view(OriDataset.AgeGroup)


#3.5 Splitting dataset with Status Active/Terminated
Active.Dataset <- subset(OriDataset.AgeGroup, Termination_Date == "1900-01-01")
Term.Dataset <- subset(OriDataset.AgeGroup, Termination_Date != "1900-01-01" & Status == "Terminated")

str(Active.Dataset)
str(Term.Dataset)
view(Active.Dataset)

#3.6 Remove Duplicated EmployeeID with Distinct()
Active.Dataset <- Active.Dataset %>%
  arrange(desc(Status_Year)) %>%
  group_by(Employee_ID) %>%
  distinct(Employee_ID, .keep_all = TRUE)%>%
  arrange(Employee_ID)

Term.Dataset <- Term.Dataset %>%
  arrange(desc(Status_Year)) %>%
  group_by(Employee_ID) %>%
  distinct(Employee_ID, .keep_all = TRUE)%>%
  arrange(Employee_ID)

str(Active.Dataset)
str(Term.Dataset)
view(Active.Dataset)
view(Term.Dataset)

#Check Result with count()
CheckAct = Active.Dataset %>% count(Employee_ID)
view(CheckAct)
CheckTerm = Term.Dataset %>% count(Employee_ID)
view(CheckTerm)

#After Group by, it changed to grouped_df, so changed back to df
NewActive.Dataset = data.frame(Active.Dataset)
NewTerm.Dataset = data.frame(Term.Dataset)

view(NewActive.Dataset)
view(NewTerm.Dataset)
str(NewActive.Dataset)
str(NewTerm.Dataset)

#3.7 Recalculating Length of Service for Term.Dataset
NewTerm.Dataset <- NewTerm.Dataset %>% 
  add_column(Length_of_Service.Year = round(as.numeric(ifelse(NewTerm.Dataset$Status == "Active", NewTerm.Dataset$Length_of_Service, 
                                                        difftime(NewTerm.Dataset$Termination_Date, 
                                                                 NewTerm.Dataset$Hire_Date,units=c("days"))/(365.25))),digits = 1 ),
             .after = "Length_of_Service")

#3.8 Eliminating Unwanted Columns
F.TermData <- subset(NewTerm.Dataset, select=-c(Gender_Desc, Length_of_Service))
F.ActiveData <- subset(NewActive.Dataset, select=-c(Gender_Desc, Term_Reason, Term_Type, Term_Year, Term_Month))
str(F.TermData)
str(F.ActiveData)


#--------------------------------------------------------------------------------#

#4.0 Data Manipulation
#4.1 Analysis 1: What is the Annual Attrition Rate?
#Calculating the Total Terminated Employees in each year
TotalTermEmpInYear <- F.TermData%>%
  group_by(Status_Year)%>%
  count()

#Calculating the Total Active Employees in each year
TotalActEmpInYear <- F.ActiveData %>%
  group_by(Status_Year)%>%
  count()

#Calculating the Annual Attrition Rate
Annual_AR = round((TotalTermEmpInYear$n/(Total_Emp))*100, digits=2)

#Creating a new data frame to plot the Attrition Rate
Year <- c(TotalTermEmpInYear$Status_Year)
Total_Emp <- TotalTermEmpInYear$n + TotalActEmpInYear$n
AttritionPlotData <- data.frame(Year, Annual_AR, Total_Emp, Total_Term_Emp=TotalTermEmpInYear$n)

#Plotting the Attrition Rate in a graph
ggplot(AttritionPlotData) +
  geom_bar(aes(x=Year, y=Total_Term_Emp), stat="identity", fill="#4F7097")+
  geom_text(aes(x=Year, y=Total_Term_Emp,label = Total_Term_Emp), size= 3, vjust=-1.0)+
  geom_line(aes(x=Year, y=100*Annual_AR),size=0.8, colour="#16C79A")+
  geom_point(aes(x=Year, y=100*Annual_AR),size=2, colour="#16C79A", alpha = 0.5)+
  geom_text(aes(x=Year, y=100*Annual_AR,label = Annual_AR), size= 3, vjust=-1.0)+
  xlim(2005,2016)+
  scale_y_continuous(sec.axis=sec_axis(
    ~.*0.01,name="Attrition Rate (%)", labels=scales::percent))+
  labs(title="Number of Terminated Employees with Attrition Rate in 2006 - 2015", x="Year", 
       y="Number of Terminated Employees")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 10))

#4.1.1 Hired employees from 2005 - 2015
HiredEmployees<- data.frame(OriDataset.AgeGroup %>% 
  arrange(desc(Status_Year)) %>%
  group_by(Employee_ID) %>%
  distinct(Employee_ID, .keep_all = TRUE)%>%
  arrange(Employee_ID)%>%
  mutate(Hire_Year = as.numeric(Hire_Year))%>%
  group_by(Hire_Year) %>%
  count()%>%
  filter(Hire_Year >=2005))

HiredEmployees %>%
  ggplot(aes(x=Hire_Year, y=n)) +
  geom_bar(stat = "identity", fill = "#30E3CA")+
  geom_text(aes(label = n), size= 3, vjust=-1.0)+
  xlim(2004,2016)+ 
  ylim(0,300)+
  labs(title="Hired Employees in 2005 -2015", x="Hire Year", y="Number of Employees")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 10))



#4.2 Reason of Attrition
#4.2.1: Termination Type by Termination Reason
TermType_Reason <- F.TermData %>%
  group_by(Term_Type, Term_Reason) %>%
  summarise(TermEmpCount = n_distinct(Employee_ID, na.rm = TRUE)) %>%
  mutate(Term_Type = factor(Term_Type),
         Term_Type = fct_reorder(Term_Type, TermEmpCount, .desc = TRUE))%>%
  ggplot(aes(x=Term_Type, y=TermEmpCount, fill=Term_Reason))+
  geom_bar(position="dodge", stat = "identity", width=0.5, colour="#FFFFFF")+
  geom_text(aes(label = TermEmpCount), size= 3, position=position_dodge(width=0.5) ,vjust=-0.5) +
  labs(title = "Termination Type with Reason", x=NULL, y= "Terminated Employee Count")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        axis.title.x = element_text(size = 10))+
  scale_fill_manual(values = c("#30E3CA","#878ECD","#BEF2FF"))


#4.2.2 Termination Reason by Department Name
Reason_Deptname <- F.TermData %>%
  group_by(Term_Reason, Department_Name) %>%
  summarise(TermEmpCount = n_distinct(Employee_ID, na.rm = TRUE)) %>%
  mutate(Department_Name  = fct_reorder2(Department_Name,TermEmpCount,Department_Name ))%>%
  ggplot(aes(x=Department_Name , y=TermEmpCount, fill=Term_Reason))+
  geom_bar( stat = "identity", width=0.5, colour="#FFFFFF")+
  geom_text(aes(label = TermEmpCount), size= 3 ,hjust=0) +
  facet_wrap(~Term_Reason)+
  scale_fill_manual(values = c("#30E3CA","#878ECD","#BEF2FF"))+
  coord_flip()+
  labs(title = "Termination Reason by Department", x= NULL, y=NULL)+
  theme_bw()+
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))


#4.2.3 Termination Reason by Top 5 Job Title with Department Name
#Term_Reason: Layoff
F.TermData %>%
  filter(Term_Reason =="Layoff")%>%
  group_by(Department_Name, Job_Title) %>%
  count()%>%
  ungroup()%>%
  top_n(5)%>%
  mutate(Job_Title  = fct_reorder(Job_Title , n, .desc=TRUE))%>%
  ggplot(aes(x=Job_Title , y=n, fill=Department_Name))+
  geom_bar(stat = "identity", width=0.5)+
  geom_text(aes(label = n), size= 3 , vjust=-0.5)+
  labs(title = "Department by Jobs in Layoff", x= NULL, y=NULL)+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

#Term_Reason: Resignation
F.TermData %>%
  filter(Term_Reason =="Resignaton")%>%
  group_by(Department_Name, Job_Title) %>%
  count()%>%
  ungroup()%>%
  top_n(5)%>%
  mutate(Job_Title  = fct_reorder(Job_Title , n, .desc=TRUE))%>%
  ggplot(aes(x=Job_Title , y=n, fill=Department_Name))+
  geom_bar(stat = "identity", width=0.5)+
  geom_text(aes(label = n), size= 3 , vjust=-0.5)+
  labs(title = "Department by Jobs in Resignation", x= NULL, y=NULL)+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

#Term_Reason: Retirement
F.TermData %>%
  filter(Term_Reason =="Retirement")%>%
  group_by(Department_Name, Job_Title) %>%
  count()%>%
  ungroup()%>%
  top_n(5)%>%
  mutate(Job_Title  = fct_reorder(Job_Title , n, .desc=TRUE))%>%
  ggplot(aes(x=Job_Title , y=n, fill=Department_Name))+
  geom_bar(stat = "identity", width=0.5)+
  geom_text(aes(label = n), size= 3 , vjust=-0.5)+
  labs(title = "Department by Jobs in Retirement", x= NULL, y=NULL)+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))


#4.3 Area 3: Analysis on jobs by Gender and Age
#4.3.1 Age Group by Gender
F.TermData %>%
  group_by(Gender,Age_Group_Desc)%>%
  count() %>%
  ggplot(aes(x= Age_Group_Desc, y = n, fill = Gender, Age_Group))+
  geom_bar(position="dodge", stat = "identity", width=0.5, colour="#FFFFFF")+
  geom_text(aes(label = n), size= 3, position=position_dodge(width=0.5) ,vjust=-0.5) +
  labs(title = "Age Group by Gender of Terminated Employee", x=NULL, y= NULL)+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        axis.text  = element_text(size = 10))

#4.3.2 Gender by Age
F.TermData %>%
  ggplot( aes(x=Age)) +
  geom_histogram(breaks=seq(0,70 , by=5), col="#e9ecef",   aes(fill=..count..)) +
  scale_fill_gradient("Count", low="#30E3CA", high="#193498")+
  facet_wrap(~Gender)+
  labs(title="Gender by Age of Terminated Employee", x="Age", y="Count")+
  theme_bw() 

#4.3.3 Gender and Age Group with Termination Reason
F.TermData %>%
  group_by(Gender,Age_Group, Term_Reason)%>%
  count()%>%
  ggplot(aes(x= Age_Group, y = n, fill = Gender))+
  geom_bar(position="dodge", stat = "identity", width=0.5, colour="#FFFFFF")+
  geom_text(aes(label = n), size= 3, position=position_dodge(width=0.5) ,vjust=-0.5) +
  labs(title = "Termination reason by Age Group and Gender", x="Age Group", y= NULL)+
  facet_wrap(~ Term_Reason)+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        axis.text  = element_text(size = 10))


#4.3.4 Gender and Age with Job Titles
F.TermData %>%
  group_by(Age,Job_Title, Gender)%>%
  count()%>%
  ungroup()%>%
  mutate(Job_Title  = fct_reorder2(Job_Title, n,Job_Title))%>%
  ggplot(aes(x= Age, y = Job_Title, size = n))+
  geom_point(aes(color = Age))+
  scale_color_gradient(low = "#FF5151", high = "#3E00FF")+
  scale_x_continuous(breaks = seq(0, 70, by = 5))+
  scale_size(range = c(1, 10), name="Count")+
  labs(title = "Job Title by Gender and Age", x=NULL, y= NULL)+
  facet_wrap(~Gender)+
  theme_bw()+
  theme(plot.title = element_text(size = 14),
        axis.title = element_text(size = 10))

  
#4.4 Area 4: Analysis of different job types and its length of service
#4.4.1 Distribution of Length of service of terminated employees
summary(F.TermData$Length_of_Service.Year)
F.TermData %>%
  ggplot(aes(x= Length_of_Service.Year))+
  geom_histogram(breaks = seq(0, 30, by=1), col="#e9ecef",   aes(fill=..count..) )+
  scale_fill_gradient("Count", low="#30E3CA", high="#193498")+
  scale_x_continuous(breaks = seq(0, 30))+
  labs(title = "The distribution of Length of Service (Years)", 
       x= "Length of Service (Years)", y="Number of Employees")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
  
#4.4.2 Average Length of Service for different Job Titles
F.TermData%>%
  group_by(Department_Name, Job_Title) %>%
  summarise(avg_Year= round(mean(Length_of_Service.Year, na.rm = TRUE)))%>%
  ungroup()%>%
  mutate(Department_Name  = fct_reorder(Department_Name, avg_Year, .desc = TRUE))%>%
  ggplot(aes(avg_Year,Department_Name,fill=Job_Title)) + 
  geom_bar(position= "dodge", stat="identity", width = 0.8, colour = "#FFFFFF")+
  geom_text(aes(label = avg_Year), size= 4, position=position_dodge(width=0.5) ,hjust=-0.5) +
  labs(title = "Avg Length of Service in Department and Job Titles", 
       x= "Avg Length of Service (Years)", y=NULL)+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


#4.4.3 Length of Service and Age by Year and Termination Reason
  F.TermData%>%
    group_by(Term_Reason, Term_Year, Age, Length_of_Service.Year)%>%
    count()%>%
    ungroup()%>%
    ggplot( aes(x=Age, y=Length_of_Service.Year)) +
    geom_point(alpha=0.5, aes(size= n, color=Term_Reason))+
    scale_x_continuous(breaks = seq(0,80, by=5))+
    scale_y_continuous(breaks = seq(0,35, by=5))+
    scale_size(range = c(1, 10), name="Count")+
    scale_color_manual(values = c("#1BF5AF","#3E00FF","#AE00FB"))+
    labs(title = "Length of Service and Age by Termination Year and Termination Reason", 
         x= "Age", y="Length of Service (Year)")+
    facet_wrap(~Term_Year)+
    theme_bw()+
    theme(
      plot.title = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10))
  
  
#4.5.1 City Name and Store Name
install.packages("treemapify")
library(treemapify)
CityTreeMap<- F.TermData%>%
  group_by(City_Name, Store_Name) %>%
  count()
  
  ggplot(CityTreeMap, aes(area = n, fill = City_Name, label = paste(City_Name, n, sep = "\n"), subgroup=Store_Name))+
  geom_treemap()+
  geom_treemap_text(colour = "black", place= "topleft", size = 15, fontface = "bold") +
  theme_bw()+
  theme(legend.position = "none")+
  geom_treemap_subgroup_border(colour = "white") +
  geom_treemap_subgroup_text(place = "centre", alpha = 0.25, colour = "black", fontface = "italic")+
  labs(title = "City Name by Store Name", x= NULL, y=NULL)
 

#4.5.2 City Name and Department by Business Unit
F.TermData%>%
  group_by( Business_Unit, Department_Name) %>%
  count()%>%
  ungroup()%>%
  mutate(Department_Name = fct_reorder(Department_Name, n))%>%
  ggplot(aes(n,Department_Name, fill= Business_Unit))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n), size= 3, position=position_dodge(width=0.5) ,hjust=-0.5)+
  labs(title = "Department by Business Unit", x= NULL, y=NULL)+
  theme_bw()

F.TermData%>%
  group_by( City_Name, Business_Unit) %>%
  count()%>%
  ungroup()%>%
  mutate(City_Name = fct_reorder(City_Name, n))%>%
  ggplot(aes(n,City_Name, fill= Business_Unit))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n), size= 4,hjust=-0.5)+
  labs(title = "City Name by Business Unit", x= NULL, y=NULL)+
  theme_bw()


#4.5.3 City Name by Termination Reason
F.TermData%>%
  group_by( City_Name, Term_Reason) %>%
  count()%>%
  ungroup()%>%
  mutate(City_Name = fct_reorder(City_Name, n))%>%
  ggplot(aes(n,City_Name, fill= Term_Reason))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n), size= 3,hjust=-0.01)+
  labs(title = "City Name by Business Unit", x= NULL, y=NULL)+
  facet_wrap(~Term_Reason)+
  theme_bw()+
  theme(legend.position  = "none",
    plot.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))


#4.5.3.1 Drill down on Fort Nelson with laid-off Job Titles
  F.TermData%>%
    filter(City_Name == "Fort Nelson", Term_Reason =="Layoff")%>%
    group_by(Job_Title) %>%
    count()%>%
    ungroup()%>%
    mutate(Job_Title = fct_reorder(Job_Title, n))%>%
    ggplot(aes(n,Job_Title))+
    geom_bar(stat = "identity", fill="#193498")+
    geom_text(aes(label = n), size= 3,hjust=-0.5)+
    labs(title = "Lay off Jobs in Fort Nelson", x= NULL, y=NULL)+
    scale_x_continuous(breaks = seq(0,10, by=1))+
    theme_bw()
  
#4.5.3.2 Drill down on Vancouver with retired and resigned Job Titles
  F.TermData %>%
    filter(City_Name == "Vancouver", Term_Reason !="Layoff")%>%
    group_by(Job_Title, Term_Reason) %>%
    count() %>%
    ungroup() %>%
    mutate(Job_Title = fct_reorder(Job_Title, n))%>%
    ggplot(aes(n,Job_Title, fill= Term_Reason))+
    geom_bar(position= "dodge",stat = "identity")+
    geom_text(aes(label = n),position=position_dodge(width = 1.0), size= 3,hjust=-0.5)+
    labs(title = "Retired and Resigned Jobs in Vancouver", x= NULL, y=NULL)+
    theme_bw()+
    theme(legend.position  = "bottom",
          plot.title = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10))


#--------------------------------------------------------------------------------#

#5.0 Extra Features
#5.1 Treemap with Treemapify package
# Reference to 4.5.1 City Name and Store Name
# Treemap allows to visualize hierarchical data in the form of nested rectangles.
# It can often be seen as a tree structure with group and subgroup.
# View the categorical data.
# Can optimize the use of space than bar chart by showing two categorical data values in the graph. 
# Able to display multiple elements together with the use of size and colour for visual effects.
  

#5.2 Fct_reorder() in Forcats package
#Reference to 4.2.3 Termination Reason by Top 5 Job Title with Department Name
# fct_reorder() help to reorder values in ascending or descending order.
# The forcats package is created to solve common problems with factors or categorical values
# In this analysis, the fct_reorder() function is mostly used in reordering the data values or x and y axis of a bar chart. 

  
#5.3 Cut() and Difftime()
# the cut function is used to create age group where it is useful in categorizing values of a continuous attribute like Age 
# The break argument is to specify the number of interval levels in separating the age values into different categories
# labels argument is to label the categorical levels

# The Difftime function is used to calculate the difference between two date variables
# The units arguement can be specified with different types of date difference like hours, days and weeks.
# In this analysis, the difftime function is used to recalculate the length of services with years and months. 
# This can be found by specifying units = c("days") then divided by 365.25 to include the leap year.
 

