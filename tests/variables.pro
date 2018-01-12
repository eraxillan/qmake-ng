UAP_CAPABILITIES += \
            appointments \
            blockedChatMessages \
            chat \
            contacts \
            enterpriseAuthentication \
            # internetClient is special, as it needs to be written without namespace
            #internetClient \
            musicLibrary \
            objects3D \
            phoneCall \
            picturesLibrary \
            removableStorage \
            sharedUserCertificates \
            userAccountInformation \
            videosLibrary \
            voipCall

BUILDS.$$size(priority) = $$key

