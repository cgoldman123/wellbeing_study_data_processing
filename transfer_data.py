# !/usr/bin/python
import sys, os, re, shutil

def get_root():
    if sys.platform == "linux":
        return "/media/labs/"
    elif sys.platform == "win32":
        return "L:/"
    elif sys.platform == "darwin":
        return "/Volumes/Labs/"

def pull_files(dir):
    flist=[]
    for root, folds, files in os.walk(dir,topdown=True):
        for ff in files:
            matchobj = re.search('(BEH.csv|preRun)', os.path.join(root, ff))
            if bool(matchobj):
                flist.append(matchobj.string)
    return flist           

def pull_ids(files):
    idlist=[]
    for ff in files:
        matchobj = re.search('[A-Z]{2}[0-9]{3}', ff)
        if bool(matchobj) and matchobj.group() not in idlist:
            idlist.append(matchobj.group())
    return idlist        
 
flist = pull_files(f'{get_root()}NPC/DataSink/WB/data-original/behavioral_session')

for subject in pull_ids(pull_files(f'{get_root()}NPC/DataSink/WB/data-original/behavioral_session')):
    if not os.path.exists(f'{get_root()}rsmith/wellbeing/data/raw/sub-{subject}'):
        os.mkdir(f'{get_root()}rsmith/wellbeing/data/raw/sub-{subject}')
    r = re.compile(f".*{subject}")
    tlist = list(filter(r.match,flist))
    
    for path in tlist:
        res = path.split(subject,1)
        if not os.path.exists(f'{get_root()}rsmith/wellbeing/data/raw/sub-{subject}/{subject}{res[1]}'):
            shutil.copy(path, f'{get_root()}rsmith/wellbeing/data/raw/sub-{subject}/{subject}{res[1]}')
    

