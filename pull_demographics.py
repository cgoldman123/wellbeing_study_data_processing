import requests, json, os, time,random
import pandas as pd

if os.name == "nt":
    root = 'L:'
elif os.name == "posix":
    root = '/media/labs'

os.chdir(f'{root}/rsmith/wellbeing/tasks/QC')

demographics = pd.DataFrame(columns=['Submission id', 'Participant id', 'Status', 'Custom study tncs accepted at', 'Started at', 'Completed at', 'Reviewed at', 'Archived at', 'Time taken', 'Completion code', 'Total approvals', 'Harmful content', 'Age', 'Sex', 'Ethnicity simplified', 'Country of birth', 'Country of residence', 'Nationality', 'Language', 'Student status', 'Employment status'])

with open('carter_prolific_api_token.txt', 'r') as file:
    carter_api_token = file.read().strip()

workspace_id = "65d650fc6fa9e61dfa165fc5"

headers = {
    "Authorization": f"Token {carter_api_token}"
}

# dictionary contains session number: (session project ID, session name, session participant group ID, approve list file location)
studies_test = {
    1:('65d66952f35ede804f6f1ca0','faces','',''),
    2:('65de0c2c245a479fe702a5f0','advice','',''),
    3:('65de0c528a9796c86e13c004','dating_and_ToM', '',''),
    4:('65de0c74c6a333c33c1a128c','social', '',''),
    5:('65de0c88fd62728364292018','cooperation', '',''),
}

studies_usa = {
    1:('6616a502839152e36a068a6b','faces','','./approve_lists/usa_approve_list_CB1/usa_approved_participants_session1_CB1.csv'),
    2:('6616ad447bf1d2e9056df906','advice','6616a62c94ee081cf6c8f51d','./approve_lists/usa_approve_list_CB1/usa_approved_participants_session2_CB1.csv'),
    3:('6616ad8288fe3363982de0f9','dating_and_ToM', '6616a6632e30de86b9b4538e','./approve_lists/usa_approve_list_CB1/usa_approved_participants_session3_CB1.csv'),
    4:('6616adbb109058fc7983755e','social', '6616a67bdd5257ab64013d26','./approve_lists/usa_approve_list_CB1/usa_approved_participants_session4_CB1.csv'),
    5:('6616ade849aa93dd9575a50c','cooperation', '6616a68b0d40ff613474ee76','./approve_lists/usa_approve_list_CB1/usa_approved_participants_session5_CB1.csv'),
}
studies_usa_cb2 = {
    1:('6644d500cd4ebdb4daf80c41','faces','','./approve_lists/usa_approve_list_CB2/usa_approved_participants_session1_CB2.csv'),
    2:('6644d57076eb22dadb502204','advice', '6644d89fc37c441dc3ebeada','./approve_lists/usa_approve_list_CB2/usa_approved_participants_session2_CB2.csv'),
    3:('6644d5ed2f2b5fe97122d37a','dating_and_ToM', '6644d8bb34bde12c5b47931d','./approve_lists/usa_approve_list_CB2/usa_approved_participants_session3_CB2.csv'),
    4:('6644d696dc67e4e407dca7d3','social', '6644d8de9268376a724f7cea','./approve_lists/usa_approve_list_CB2/usa_approved_participants_session4_CB2.csv'),
    5:('6644d7e233e81f8b16595896','cooperation', '6644d8fe8122109317c47f4b','./approve_lists/usa_approve_list_CB2/usa_approved_participants_session5_CB2.csv'),
}
studies_japan = {
    1:('6638fff29189dc7a7b72b504','faces','','./approve_lists/japan_approve_list_CB1/japan_approved_participants_session1_CB1.csv'),
    2:('664b5d0be46bae63ae071549','advice','6644f8e7a5e1622e5cf8fbc0','./approve_lists/japan_approve_list_CB1/japan_approved_participants_session2_CB1.csv'),
    3:('664b73cecce9861577829e90','dating_and_ToM', '6644f8f36f8e8cb0424a398f','./approve_lists/japan_approve_list_CB1/japan_approved_participants_session3_CB1.csv'),
    4:('664b74d8ce8bcfd0240d292a','social', '6644f905856dd858b521b3ca','./approve_lists/japan_approve_list_CB1/japan_approved_participants_session4_CB1.csv'),
    5:('664b75563199507f9ffced7b','cooperation', '6644f90f7ad90c49eff24542','./approve_lists/japan_approve_list_CB1/japan_approved_participants_session5_CB1.csv'),
}
studies_japanese = {
    1:('665649a6eb9a4519439281cd','faces','','./approve_lists/japanese_approve_list_CB1/japanese_approved_participants_session1_CB1.csv'),
    2:('665733da63de0137bb9a86e7','advice','66573308a1a5b0631cdd509b','./approve_lists/japanese_approve_list_CB1/japanese_approved_participants_session2_CB1.csv'),
    3:('665734cc660336b21d8e7213','dating_and_ToM', '66573316ff4736e46b398b74','./approve_lists/japanese_approve_list_CB1/japanese_approved_participants_session3_CB1.csv'),
    4:('665735477b759c5812413dcb','social', '6657339b7b3a31deecb07c03','./approve_lists/japanese_approve_list_CB1/japanese_approved_participants_session4_CB1.csv'),
    5:('665735ae1fbdc0b592ef912f','cooperation', '665733ac55e399bec6fe619c','./approve_lists/japanese_approve_list_CB1/japanese_approved_participants_session5_CB1.csv'),
}
studies_usa_cb1_r2 = {
    1:('665a36125a0f3c3cb092f567','cooperation','',''),
    2:('6659f73d9faab34ab2381a20','social','6659e3d0271bc74e6841d273',''),
    3:('6659f9bfabab1518dad222a9','dating_and_ToM', '6659e3e2f7f959147c664784',''),
    4:('6659fa44f69c2ea78f5fe1b3','advice', '6659e3efdd39d2f11487698c',''),
    5:('6659fcbe99e85b0ec6bafce5','faces', '6659e3fb45ae71208d7f7ba3',''),
}
studies_usa_cb2_r2 = {
    1:('6659fe56761cd977cc800be8','cooperation','',''),
    2:('6659fecaa9388e7971e5080c','social','6659ff17384b160d2ec963bb',''),
    3:('6659ffb9c62d40dd7738c510','dating_and_ToM', '6659ff283b2af336e6b6d3ab',''),
    4:('665a0015b0a7c3c3080fa304','advice', '6659ff333031c46f7df9a7e5',''),
    5:('665a00a0d602bed25511a2f9','faces', '6659ff41566ce6a7ff2d5e97',''),
}
studies_asia_cb1_r1 = {
    1:('665f369edd629ea90acc81a6','faces','',''), # using corrected project ID
    2:('665a225d99b96e515e1e51aa','advice','665a2335abaffb1117c59cc8',''),
    3:('665a262961819c47de502de0','dating_and_ToM', '665a233efc18959d6c8f4a58',''),
    4:('665a2662aea810cb8ff4c844','social', '665a234af36150654ab32d83',''),
    5:('665a2697344945410389820a','cooperation', '665a23535f683f4d0ed647a8',''),
}
studies_asia_cb2_r1 = {
    1:('665f36149054be030ed517c0','faces','',''), # using correctted project ID
    2:('665a27b25cdab4876b2d6704','advice','665a238d07dc7c6c8f2f679d',''),
    3:('665a27e9f8a77b62563b0191','dating_and_ToM', '665a2398152454bb2e630630',''),
    4:('665a280ad5e0e7f47c7fdfe2','social', '665a23a0ecf98c5daa3cbc63',''),
    5:('665a2843c79e6bed8c56cb39','cooperation', '665a23aa57546a49a3fc9508',''),
}
studies_asia_cb1_r2 = {
    1:('665f365b20162e9ac943c233','cooperation','',''), # using corrected project ID
    2:('665a2ac14d8b38f6682d9ec6','social','665a23cd77c7b504ed7d125d',''),
    3:('665a2b01a74f01a774740cd6','dating_and_ToM', '665a23d8cec6848f16a00975',''),
    4:('665a2b376c9708bd900a9bec','advice', '665a24e9c20a6f66b592f59d',''),
    5:('665a2b679aa44d8ecbb1c4b4','faces', '665a24ff0f473bd47c34d578',''),
}
studies_asia_cb2_r2 = {
    1:('665a2ba01d10972d59422c0b','cooperation','',''),
    2:('665a2dad7630dc2ff040cdbb','social','665a2529640487f7dd2b2819',''),
    3:('665a2dde32a1b5618efbae8c','dating_and_ToM', '665a2546d5c023bf4776ee2a',''),
    4:('665a2e21a990540cf8be84fd','advice', '665a255257097d4fb5a00afa',''),
    5:('665a2e44a0698cd45b233d4c','faces', '665a255da0c87db18a2693f2',''),
}

asia_old_cb1_r1 = {
    1:('6659dad59c57e4405d3de08b','faces','',''), # using old project ID
}
asia_old_cb2_r1 = {
    1:('665a275628adee6320f6690d','faces','',''), # using old project ID
}
asia_old_cb1_r2 = {
    1:('6659e0e9007ca80f50094e54','cooperation','',''), # using old project ID
}

def get_demos(study,studies,p_name):
    global demographics
    project_id = studies[study][0]
    project_name = studies[study][1]
   
    # export demographic data and save to csv
    demos = requests.get(f"https://api.prolific.com/api/v1/studies/{project_id}/export", headers=headers)
    if demos.ok:
        #print(f"Demographic data retrieved successfully for {p_name}")
        f = open("../../data/prolific/demographics_temp.txt", "w")
        f.write(demos.text)
        f.close()

        df = pd.read_csv("../../data/prolific/demographics_temp.txt")
        df['R'] =p_name.split('_')[0]
        df['CB'] = p_name.split('_')[1]
        df['Culture'] = p_name.split('_')[2]
        df['Session'] = study

        demographics = pd.concat([demographics,df])
        
def check_usa_test():
    for study in studies_test:
        get_demos(study,studies_test,'R1_CB1_USATest')

def check_usa_cb1():
    for study in studies_usa:
        get_demos(study,studies_usa,'R1_CB1_USA')

def check_usa_cb2():
    for study in studies_usa_cb2:
        get_demos(study,studies_usa_cb2,'R1_CB2_USA')

def check_japan_cb1():
    for study in studies_japan:
        get_demos(study,studies_japan, 'R1_CB1_Japan')

def check_japanese_cb1():
    for study in studies_japanese:
        get_demos(study,studies_japanese, 'R1_CB1_Japanese')

def check_usa_cb1_r2():
    for study in studies_usa_cb1_r2:
        get_demos(study,studies_usa_cb1_r2, 'R2_CB1_USA')

def check_usa_cb2_r2():
    for study in studies_usa_cb2_r2:
        get_demos(study,studies_usa_cb2_r2, 'R2_CB2_USA')

def check_asia_cb1_r1():
    for study in studies_asia_cb1_r1:
        get_demos(study,studies_asia_cb1_r1, 'R1_CB1_Asia')

def check_asia_cb2_r1():
    for study in studies_asia_cb2_r1:
        get_demos(study,studies_asia_cb2_r1, 'R1_CB2_Asia')

def check_asia_cb1_r2():
    for study in studies_asia_cb1_r2:
        get_demos(study,studies_asia_cb1_r2, 'R2_CB1_Asia')

def check_asia_cb2_r2():
    for study in studies_asia_cb2_r2:
        get_demos(study,studies_asia_cb2_r2, 'R2_CB2_Asia')

def check_old_cb1_r1():
    for study in asia_old_cb1_r1:
        get_demos(study,asia_old_cb1_r1, 'R1_CB1_AsiaOld')
def check_old_cb2_r1():
    for study in asia_old_cb2_r1:
        get_demos(study,asia_old_cb2_r1, 'R1_CB2_AsiaOld')

def check_old_cb1_r2():
    for study in asia_old_cb1_r2:
        get_demos(study,asia_old_cb1_r2, 'R2_CB1_AsiaOld')        
# List of functions
functions = [
    check_usa_test,
    check_usa_cb1,
    check_usa_cb2,
    check_japan_cb1,
    check_japanese_cb1,
    check_usa_cb1_r2,
    check_usa_cb2_r2,
    check_asia_cb1_r1,
    check_asia_cb2_r1,
    check_asia_cb1_r2,
    check_asia_cb2_r2,
    check_old_cb1_r1,
    check_old_cb2_r1,
    check_old_cb1_r2
]


# Execute each function in the shuffled list
for func in functions:
    func()
demographics.to_csv(f'../../data/prolific/all_demographic_data.csv', index=None)