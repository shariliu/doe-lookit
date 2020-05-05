import os
import subprocess as sp
import sys

videoPath = '/Users/shariliu/Documents/HarvardLDS/Studies/DOE-lookit/stim/mp4/'

videoFiles = os.listdir(videoPath)

for video in videoFiles:
    (shortname, ext) = os.path.splitext(video)

    print(shortname)

    if ext in ['.mp4']:

    	sp.call(['ffmpeg', '-i', os.path.join(videoPath, video), \
    		'-vcodec', 'libvpx', '-acodec', 'libvorbis', os.path.join(videoPath, shortname + '.webm')])