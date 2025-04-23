

import os
import numpy as np
import pandas as pd
import re
import statsmodels.formula.api as smf
from mne.stats import bonferroni_correction
pd.options.mode.chained_assignment = None  # default='warn'

numbers = re.compile(r'(\d+)') #用于匹配数字，用于编译正则表达式，生成一个Pattern对象，供其他函数使用
def numericalSort(value):
    parts = numbers.split(value)
    parts[1::2] = map(int, parts[1::2])
    return parts

def get_normalization(mydf): #规范化函数
    tmp_df = mydf.copy()
    for col in tmp_df.columns:
        tmp_df[col] = (tmp_df[col]-tmp_df[col].mean()) / tmp_df[col].std()
    return tmp_df

def get_binarization(mydf): #哑变量处理
    tmp_df = pd.get_dummies(mydf).iloc[:, 1]
    return tmp_df

def preprocess_df(mydf, target_f): #对不同的变量进行不同类型的处理
    tmp_df = mydf.copy()
    #tmp_df.columns = tmp_df.columns.tolist()[:7] + ['Ethnicity_Others', 'Ethnicity_White', 'Ethnicity_Asian', 'Ethnicity_Black']
    # normalize if it is not binarized variable
    if len(tmp_df[target_f].value_counts()) > 2:
        tmp_df[target_f] = get_normalization(tmp_df[[target_f]])
        # remove missing values (row manipulation)
        tmp_df.dropna(axis=0, inplace=True)
    elif len(tmp_df[target_f].value_counts()) == 2:
        # remove missing values (row manipulation)
        tmp_df.dropna(axis=0, inplace=True)
        tmp_df[target_f] = get_binarization(tmp_df[target_f])
    # remove without information (levels < 2) (column manipulation)
    #rm_cols = [col for col in tmp_df.columns if len(tmp_df[col].value_counts()) <= 1]
    #tmp_df.drop(rm_cols, axis = 1, inplace = True)
    return tmp_df

def read_preprocessed_df(file_path):
    mydf = pd.read_csv(file_path)
    mydf[['Age', 'BMI', 'Education', 'TD_Index']] = get_normalization(mydf[['Age', 'BMI', 'Education', 'TD_Index']])
    return mydf

Work_path = '/Users/panguoqing/Desktop/help/project/'
dpath = Work_path + 'Data/'
f_df = pd.read_csv(dpath + 'PhenoData/Feature_Dict.csv')
f_category = pd.read_csv(dpath + 'PhenoData/feat_catorgy.csv')
f_category = f_category[['FieldID_full', 'Feature_Category']]

MD_list = ["AX", "BIP", "DEP", "SCH"]
for target in MD_list:

    target_yrs = 'BL2' + target + '_yrs'
    outpath = dpath + 'CaseControl/' + target
    out_file_multi = outpath + '/Pvalue_beta.csv'
    file = outpath + '/data_cov_pheno_forPvalue.csv'

    # tmp = read_preprocessed_df(file)
    # all_result_df_multi = pd.DataFrame({'FieldID_full': tmp.columns[61:]})
    # suffix = os.path.basename(file).split('_')[1].split('.')[0]

    mydf = read_preprocessed_df(file)
    #mydf = mydf[(mydf[target_yrs] >= i) & (mydf[target_yrs] < i + 10)]
    my_f = mydf.columns.tolist()[62:]
    n = len(my_f)

    result_df_multi = pd.DataFrame()
    for i in range(0, n):
        try:
            print(i)
            f = my_f[i]
            tmp_df = mydf[
                [f] + ['group', 'duration', 'Age', 'Gender', 'Education', 'TD_Index', 'Ethnicity', 'Sites', 'BMI', 'SMK_Status', 'ALC_Status', 'icosatiles',
                       'age_gap', 'fasting_time', 'time_of_sampling', 'month_of_assessment', 'day_of_assay']]
            tmp_df = preprocess_df(tmp_df, target_f=f)
            mod_multi = smf.ols(
                formula = 'tmp_df[f] ~ group*duration + group:I(duration ** 2) + Age + C(Gender)*C(Ethnicity) + C(Gender):C(age_gap) + icosatiles + C(Sites) + C(Education) + TD_Index  + BMI + C(SMK_Status) + C(ALC_Status) + C(time_of_sampling) + C(fasting_time) + C(month_of_assessment) + C(day_of_assay)',
                data=tmp_df
            ).fit()
            #
            p_group = mod_multi.pvalues['group']
            p_group_time = mod_multi.pvalues['group:duration']
            p_group_time2 = mod_multi.pvalues['group:I(duration ** 2)']
            beta_group = mod_multi.params['group']
            beta_group_time = mod_multi.pvalues['group:duration']
            beta_group_time2 = mod_multi.pvalues['group:I(duration ** 2)']
            p_multi = [f, p_group, p_group_time, p_group_time2, beta_group, beta_group_time, beta_group_time2]
            result_df_multi = pd.concat((result_df_multi, pd.DataFrame(p_multi).T), axis=0)
        except:
            pass

    result_df_multi.columns = ['FieldID_full',  'p_group', 'p_group_time', 'p_group_time2', 'beta_group', 'beta_group_time', 'beta_group_time2']
    #all_result_df_multi = pd.merge(all_result_df_multi, result_df_multi, how='left', on=['FieldID_full'])

    #Field_lst = result_df_multi['FieldID_full']
    #Field_df = pd.DataFrame(Field_lst)
    p_group_lst = result_df_multi['p_group']
    p_group_time_lst = result_df_multi['p_group_time']
    p_group_time2_lst = result_df_multi['p_group_time2']

    reject1_case, p_group_bfi = bonferroni_correction(p_group_lst.fillna(1), alpha=0.05)
    reject2_case, p_group_time_bfi = bonferroni_correction(p_group_time_lst.fillna(1), alpha=0.05)
    reject3_case, p_group_time2_bfi = bonferroni_correction(p_group_time2_lst.fillna(1), alpha=0.05)

    p_group_df = pd.DataFrame(p_group_bfi)
    p_group_df.columns = ['p_group_bfi']
    p_group_time_df = pd.DataFrame(p_group_time_bfi)
    p_group_time_df.columns = ['p_group_time_bfi']
    p_group_time2_df = pd.DataFrame(p_group_time2_bfi)
    p_group_time2_df.columns = ['p_group_time2_bfi']
    result_df_multi.to_csv(out_file_multi, index=False)

    result_df_multi1 = pd.read_csv(out_file_multi)
    result_df_multi1 = pd.concat([result_df_multi1, p_group_df], axis=1)
    result_df_multi1 = pd.concat([result_df_multi1, p_group_time_df], axis=1)
    result_df_multi1 = pd.concat([result_df_multi1, p_group_time2_df], axis=1)

    result_df_multi1.to_csv(out_file_multi, index=False)
