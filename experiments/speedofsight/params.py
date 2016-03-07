#! /usr/local/bin/python2.5

from psychopy import gui
from random import random, shuffle
from os import path, listdir

def get_pinfo():
    """
    Parameters
        vision: normal or corrected to normal vision?                                                                                                                                            
        age:                                                                                                                                                                                     
        gender:                                                                                                                                                                                  
        hand:                                                                                                                                                                                    
        particip: participated before?                                                                                                                                                           
        familiar: familiar with the procedure?   
    """    
    
    info = {'Is your vision normal or corrected-to-normal?':'',
            'What is your Gender?':'',
            'What is your Age?':'',
            'How many times have you participated?':'',
            'Are you familiar with the image set?':''}

    infoDlg = gui.DlgFromDict(dictionary=info)
    
    pinfo = dict(zip(['gender', 'particip', 'age', 'familiar', 'vision'],
                     [str(elem) for elem in info.values()]))

    return pinfo


def get_protocol(img_path,extensions,trials):
    img_path = path.abspath(img_path)
    tmp = range(trials)
    shuffle(tmp)
    protocol = {
        'num_soa'  : 4 ,            # number of soas
        'pos_key'  : 'RightShift',
        'neg_key'  : 'LeftShift',
        'res_time' : [0]*trials,            # response time 
        'target'   : [elem for elem in listdir(img_path +'/targets') 
                      if elem[-3:] in extensions],
        'mask'     : [elem for elem in listdir(img_path +'/masks')
                      if elem[-3:] in extensions],
        'ndx'      : tmp,
        'tar_flag' : [None]*trials,
        'soa'      : [0]*trials,
        'choice'   : [None]*trials,
        }
    
    protocol['soa'] = [int(elem[9]) for elem in protocol['target']]
    protocol['tar_flag'] = [elem[11].isdigit() for elem in protocol['target']]

   # for i in range(protocol['target'].__len__()):
   #     if protocol['target'][i][11].isalpha(): 
   #         protocol['tar_flag'].append(0)    
   #     else:
   #         protocol['tar_flag'].append(1)
   #                
   #     protocol['soa'].append(int(protocol['target'][i][9]))


    if random() > .5:
        protocol['pos_key'] = 'LeftShift'
        protocol['neg_key'] = 'RightShift'
        
        
    return protocol 

#myDlg.show()

#if myDlg.OK:
#thisInfo = myDlg.data
#print thisInfo
#else: print 'user canceled'
