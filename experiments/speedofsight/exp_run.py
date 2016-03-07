#! /usr/local/bin/python2.5


import sys
import pylab
import pickle
from params import *
from exp import Exp
from psychopy import visual,core


def main(img_path):
    trials = 20
    framerate = 1/60.0
    width = 1920
    height = 1200
    w = width/4
    #img_path = '/Users/sb/neuro_comp/db/num_exp'
    pinfo = get_pinfo()
    kwargs = get_protocol(img_path,['png','jpg'],trials)
    exp = Exp(**kwargs)
    win = visual.Window(screen=1,fullscr=True)
    clock = core.Clock()
    
    for i in range(trials):
        exp._run_one_trial(win,i,img_path,w,framerate,clock)
       
    
    exp_file = open('filename.obj','w')
    pickle.dump(exp,exp_file)
    pylab.plot(win.frameIntervals)
    pylab.show()




if __name__ == "__main__":
    
    img_path = sys.argv[1]
    main(img_path)

