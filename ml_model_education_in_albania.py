#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec  8 09:38:22 2022

@author: kajaadamczyk
"""
import wbdata
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
import numpy as np
import seaborn as sns
import seaborn


#exploting World Bank data
indicators={"UIS.SR.1.GLAST.GPI":"Survival rate to last grade of primary education - GPI",
       "SE.PRM.PRSL.ZS":"Persistence to last grade",
       "UIS.SCHBSP.1.WCOMPUT":"prop of schools with access to computers",
       "UIS.SCHBSP.1.WINFSTUDIS":"Prop of schools with facilities for disabled'",
       "UIS.TATTRR.1.T":"Teacher attrition rate",
       "UIS.SCHBSP.1.WELEC":"prop w access to electricity",
       "UIS.SCHBSP.1.WINTERN":"prop w access to internet",
       "UIS.TATTRR.1.GPIA":"teacher attrition GPI",
       "1.1_ACCESS.ELECTRICITY.TOT":"electAccess"
    }

attrition = {'UIS.TATTRR.1.T': '"Teacher attrition rate"'}

wb=wbdata.get_dataframe(indicators,country='ALB')


wb_pers=wbdata.get_dataframe(indicators=attrition,country='ALB')
wbu_pers = wb_pers.unstack(level=0)
wb2=wb_pers.copy() #creates a copy of dataframe; if you instead  do wb2=wb all changes to wb2 will impact wb
wb2.reset_index(inplace=True)
wb_final=wb2[(wb2['date']>'2005') & (wb2['date']<'2020')]
wb_final = wb_final.sort_values("date")


wb_final.plot('date', '"Teacher attrition rate"');
plt.title('Teacher attrition in Albania')
plt.xlabel('Date')
plt.ylabel('In %')
plt.grid()


#plotting persistence to last grade
persistence={"SE.PRM.PRSL.ZS":"Persistence to last grade"}


wb_pers=wbdata.get_dataframe(indicators=persistence,country='ALB')
wbu_pers =wb_pers.unstack(level=0)
wb2=wb_pers.copy() #creates a copy of dataframe; if you instead  do wb2=wb all changes to wb2 will impact wb
wb2.reset_index(inplace=True)
wb_final=wb2[(wb2['date']>'2005') & (wb2['date']<'2020')]
wb_final = wb_final.sort_values("date")


wb_final.plot('date', "Persistence to last grade");
plt.title('Persistence to last grade in Albania')
plt.xlabel('Date')
plt.ylabel('% of persistence')
plt.grid()
plt.box(False)
plt.savefig('/Users/kajaadamczyk/Desktop/HEDSpython-main/persistencegrid.png', transparent=True) 


#moving onto the implementation based on proxy data

df = pd.read_excel(io='~/Desktop/uni/heds_proxy_data.xlsx', sheet_name='Sheet1')

df.drop(df.tail(2).index,
        inplace = True)


df2 = df.rename(columns={'q4': 'Internet/computers', 'q3': 'Facilities for disabled'})

df2['Internet/computers'] = np.where(df2['Internet/computers'] == 1, 'Yes', 'No')
df2['Facilities for disabled'] = np.where(df2['Facilities for disabled'] == 1, 'Yes', 'No')

#adding second order polynomials
q1s = np.square(df['q1'])
df = df.assign(q1_squared=q1s)

q2s = np.square(df['q2'])
df = df.assign(q2_squared=q2s)

q5s = np.square(df['q5'])
df = df.assign(q5_squared=q5s)

#adding interactions between continuous variables
q1q2 = df['q2']*df['q1']
df = df.assign(q1_q2=q1q2)

q1q5 = df['q5']*df['q1']
df = df.assign(q1_q5=q1q5)

q2q5 = df['q2']*df['q5']
df = df.assign(q2_q5=q2q5)



#DV- persistence
#IV - q1-q5
#plot scatter plot first, correlation between questions
#q1 = % of trained teachers in primary education
#q2 = how satisfied are pupils with their teachers -> scale 0-5 (recoded to % in excel)
#q3 = facilities for disabled students? binary - 1 yes, 0 no
#q4 = does the school have access to Internet/computers for pedagogical
#purposes; binary - 1 yes, 0 no
#q5 = % of teacher persistence rate (0-100) -> 100%-attrition

#plotting correlations between all of the variables



#seaborn analysis
#https://www.tutorialspoint.com/how-to-save-a-plot-in-seaborn-with-python-matplotlib#:~:text=To%20save%20a%20plot%20in%20Seaborn%2C%20we,use%20the%20savefig()%20method.

#only predictors
only_predictors = df2.drop(['persistence', 'school ID'], axis=1)

no_q4 = only_predictors.drop('Internet/computers', axis=1)

dfs = seaborn.load_dataset('tips')
seaborn.pairplot(no_q4, hue ='Facilities for disabled')
seaborn.pairplot(no_q4, hue ='Facilities for disabled').fig.suptitle('Interactions', y=1.01)
plt.savefig('/Users/kajaadamczyk/Desktop/HEDSpython-main/q3_hue', transparent=True, bbox_inches="tight") 



no_q3 = only_predictors.drop('Facilities for disabled', axis=1)
# loading dataset using seaborn
dfs = seaborn.load_dataset('tips')
# pairplot with hue sex
seaborn.pairplot(no_q3, hue ='Internet/computers')
seaborn.pairplot(no_q3, hue ='Internet/computers').fig.suptitle('Interactions', y=1.01)
plt.savefig('/Users/kajaadamczyk/Desktop/HEDSpython-main/q4_hue', transparent=True, bbox_inches="tight") 




#moving onto estimating the linear regression model
#they are correlated - including interactions

#summary of lm 
import statsmodels.api as sm

#define response variable
y = df['persistence']

#define predictor variables
#one with all of them not working (cause of binary variables?)
#x = df[['q1', 'q2', 'Internet/computers', 'Facilities for disabled', 'q5']]


#start with all variables and then drop the most insignificant ones sequentially
#including also squares of continuous variables and interactions between the continuous ones
x = df[['q1', 'q2', 'q3','q4', 'q5','q1_squared', 'q2_squared', 'q5_squared', 
        'q1_q2', 'q1_q5', 'q2_q5']]


#add constant to predictor variables
x = sm.add_constant(x)

#fit linear regression model
model = sm.OLS(y, x).fit()

#view model summary
print(model.summary())

#will be redoing this, deleting the most insignificant variable, untill there are
#no insignificant left

#deleting q5
x = df[['q1', 'q2', 'q3','q4', 'q1_squared', 'q2_squared','q5_squared',
        'q1_q2', 'q1_q5', 'q2_q5']]


#add constant to predictor variables
x = sm.add_constant(x)

#fit linear regression model
model = sm.OLS(y, x).fit()

#view model summary
print(model.summary())

#deleting q2_squared
x = df[['q1', 'q2', 'q3','q4', 'q1_squared','q5_squared',
        'q1_q2', 'q1_q5', 'q2_q5']]



#add constant to predictor variables
x = sm.add_constant(x)

#fit linear regression model
model = sm.OLS(y, x).fit()

#view model summary
print(model.summary())

#deleting q1_squared
x = df[['q1', 'q2', 'q3','q4','q5_squared',
        'q1_q2', 'q1_q5', 'q2_q5']]


#add constant to predictor variables
x = sm.add_constant(x)

#fit linear regression model
model = sm.OLS(y, x).fit()

#view model summary
print(model.summary())

#deleting q5_squared
x = df[['q1', 'q2', 'q3','q4',
        'q1_q2', 'q1_q5', 'q2_q5']]


#add constant to predictor variables
x = sm.add_constant(x)

#fit linear regression model
model = sm.OLS(y, x).fit()

#view model summary
print(model.summary())

#deleting q1
x = df[['q2', 'q3','q4',
        'q1_q2', 'q1_q5', 'q2_q5']]


#add constant to predictor variables
x = sm.add_constant(x)

#fit linear regression model
model = sm.OLS(y, x).fit()

#view model summary
print(model.summary())

#deleting q1_q5
x = df[['q2', 'q3','q4',
        'q1_q2', 'q2_q5']]


#add constant to predictor variables
x = sm.add_constant(x)

#fit linear regression model
model = sm.OLS(y, x).fit()

#view model summary
print(model.summary())

#deleting q2_q5
x = df[['q2', 'q3','q4',
        'q1_q2']]


#add constant to predictor variables
x = sm.add_constant(x)

#fit linear regression model
model = sm.OLS(y, x).fit()

#view model summary
print(model.summary())

y = df['persistence']
#deleting q2
x = df[['q3','q4',
        'q1_q2']]

#add constant to predictor variables
x = sm.add_constant(x)

#fit linear regression model
model_final = sm.OLS(y, x).fit()

#view model summary
print(model_final.summary())
#adjusted R^2 = ~76%, R^2 = 77%

#all statistically significant

#plot statistically significant values against persistence
#binary diff than continous

#plotting q3's relationship with persistence
ax = seaborn.kdeplot(data=df2, x="persistence", hue="Facilities for disabled", 
                fill=True, common_norm=False, alpha=0.4)
seaborn.move_legend(ax, "upper left")
plt.box(False)
plt.xlabel('Persistence')
plt.grid()
plt.title('Correlation between facilities for disabled and persistence')
#plt.show()
plt.savefig('/Users/kajaadamczyk/Desktop/HEDSpython-main/q3_pers', bbox_inches='tight',
            transparent=True) 

#plotting q4's relationship with persistence
bx = seaborn.kdeplot(data=df2, x="persistence", hue="Internet/computers", 
                fill=True, common_norm=False, alpha=0.4)
plt.xlabel('Persistence')
seaborn.move_legend(bx, "upper right")
plt.box(False)
plt.grid()
plt.title('Correlation between Internet/computers and persistence')
#plt.show()
plt.savefig('/Users/kajaadamczyk/Desktop/HEDSpython-main/q4_pers', bbox_inches='tight',
            transparent=True) 

#plotting q1_q2 relationship with persistence
sns.lmplot('q1_q2', 'persistence', df, fit_reg=False)
fig = plt.gcf()
fig.set_size_inches(5,5)
plt.xlabel('% of trained teachers and students satisfaction (interaction term)')
plt.ylabel('Persistence')
plt.title('Correlation between Q1 & Q2 (interaction term) and persistence')
plt.grid()
plt.box(False)
#plt.show()
plt.savefig('/Users/kajaadamczyk/Desktop/HEDSpython-main/q1_q2_pers', bbox_inches='tight',
            transparent=True) 



#train model only with statistically significant variables: q1_q2, q3, q4


x = df[['q1_q2', 'q3', 'q4']]
y = df['persistence']

#splitting data: 20% train, 80% test
x_train, x_test, y_train, y_test = train_test_split(x,y,test_size=0.2)

#lineaar regression
regressionModel = LinearRegression()
regressionModel.fit(x_train, y_train)

# Create Prediction
y_prediction= regressionModel.predict(x_test)
y_pred = y_prediction.reshape(1,-1)

# Evaluate the model
confidence = regressionModel.score(x,y)
print('confidence', confidence)
#confidence = ~77%, which is a pretty good result


#plot predicted vs actual values
plt.figure(figsize=(10,10))
plt.scatter(y_test, y_prediction, c='crimson')


p1 = max(max(y_prediction), max(y_test))
p2 = min(min(y_prediction), min(y_test))
plt.plot([p1, p2], [p1, p2], 'b-')
plt.xlabel('True Values', fontsize=15)
plt.ylabel('Predictions', fontsize=15)
plt.axis('equal')
plt.title("The accuracy of the predictive model", fontsize=17)
plt.grid()
plt.box(False)
#plt.show()
plt.savefig('/Users/kajaadamczyk/Desktop/HEDSpython-main/pred_actual', bbox_inches='tight',
            transparent=True) 





