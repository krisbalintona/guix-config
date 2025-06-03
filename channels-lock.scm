(list (channel
        (name 'emacs-master)
        (url "https://github.com/gs-101/emacs-master.git")
        (branch "main")
        (commit
          "68bb075f860d5eb138bbcadaadd57381a7822c29")
        (introduction
          (make-channel-introduction
            "568579841d0ca41a9d222a2cfcad9a7367f9073b"
            (openpgp-fingerprint
              "3049 BF6C 0829 94E4 38ED  4A15 3033 E0E9 F7E2 5FE4"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "02270b585e7d641afabd86ee6eaf7a6aad2f5df7")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "ad8cb7af8fc572bb3d11cd0344bd2bed3b9d653f")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
