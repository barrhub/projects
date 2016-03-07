#! /usr/local/bin/python2.5  
from psychopy import * 

class Exp(object):

    """ 
    Exp is a class containing experimental parameters
    
    """

    def __init__(self, **kwargs):
    
        """ Constructor
    
        kwargs (keyword arguments)
            seed: seed for random number generator
            pos_key: yes key
            neg_key: no key
            tar_flag: was there a target present? 1 yes, 0 no
            choice: was a target reported? 1 yes, 0 no
            resp_time: rough interval between target offset and response
            pairs: target, mask name
            ndx: target, mask file index
            soa: interval between target onset and mask onset
        
        """
        self.__dict__.update(kwargs)
    
    def _run_one_trial(self,win,trial,img_path,w,fr,clock):
        test = True
        targ = self.target[self.ndx[trial]]
        dist = self.target[self.ndx[trial]-15]
        mask = self.mask[self.ndx[trial]]
        targP = img_path + '/targets/' + targ
        distP = img_path + '/targets/' + dist
        maskP = img_path + '/masks/' + mask
        if test:
            im_b = 'path/to/some/black/image'
            im_w = 'path/to/some/white/image'

        soa = self.soa[self.ndx[trial]]
        wait = fr*soa
        print 'target: ' + targ + '  mask: ' + mask + '  ndx: ' + str(self.ndx[trial])
        
        targS = visual.SimpleImageStim(win,image=targP,pos=(100,0))
        distS = visual.SimpleImageStim(win,image=distP,pos=(-100,0))
        maskL = visual.SimpleImageStim(win,image=maskP,pos=(-100,0))
        maskR = visual.SimpleImageStim(win,image=maskP,pos=(100,0))
        
        if test:
            black = visual.SimpleImageStim(win,image=im_b, pos=(768,512))
            white = visual.SimpleImageStim(win,image=im_w, pos=(768,512))

        win.setRecordFrameIntervals(value=True)

        targS.draw()
        distS.draw()
        if test:
            black.draw()
        win.flip()
        t0 = clock.getTime()        
        maskL.draw()
        maskR.draw()
        
        if test:
            white.draw()
        core.wait(wait)
        win.flip()
        t1 = clock.getTime()
        win.setRecordFrameIntervals(value=False)

        allKeys = event.waitKeys()
        print allKeys
        tmp = t1-t0
        print 'target: '+str(tmp)
        if tmp > wait:
            miss += 1
        print 'miss: '+str(miss) 
        
        
        
    
    
    
