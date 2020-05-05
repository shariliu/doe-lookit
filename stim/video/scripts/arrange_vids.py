import os
import subprocess as sp
import sys

videoPath = '/Users/shariliu/Documents/HarvardLDS/Studies/DOE-lookit/stim/video/origs/'

videoFiles = os.listdir(videoPath)

# hab video
sp.call(["ffmpeg", "-i", "hab_orig_crop_trim.mp4", "-i", "hab_orig_crop_trim_flip.mp4", "-filter_complex",\
 "[0]pad=iw+500:color=white[left];[left][1]hstack=inputs=2", "hab.mp4"])

# test video 1
sp.call(["ffmpeg", "-i", "high_orig_crop_trim.mp4", "-i", "low_orig_crop_trim_flip.mp4", "-filter_complex",\
 "[0]pad=iw+500:color=white[left];[left][1]hstack=inputs=2", "ineff_eff.mp4"])

# test video 2
sp.call(["ffmpeg", "-i", "low_orig_crop_trim.mp4", "-i", "high_orig_crop_trim_flip.mp4", "-filter_complex",\
 "[0]pad=iw+500:color=white[left];[left][1]hstack=inputs=2", "eff_ineff.mp4"])