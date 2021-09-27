# Scattering convolutional networks for image classification/analysis

> Contact : Marco Corneli (Marco.CORNELI@univ-cotedazur.fr) Description

The main ingredient of a scattering transform is a cascade of wavelets, use to convolve the input image(s) to produce some descriptors being invariant to isometries (rotation, translation, etc.) and small deformations. In this sense, scattering transforms can be seen as convolutional neural networks (CNNs) with fixed predetermined filters. Scattering-based models have been shown useful in several applications (Sifre and Mallat, 2013; Oyallon et al., 2018) especially involving limited labelled images. Indeed, the scattering layers have a few parameters fixed by the user and do not need to be learned, which is useful when dealing with small datasets. More recently, also scattering layers were optimized via back-propagation (Gauthier et al., 2021), thus representing a viable alternative to more complex CNN architectures, especially for small datasets classification.

## Suggestions
The student is encouraged to start from the paper Gauthier et al. (2021), which is clear in describing how scattering networks work. Then, it will be wise to collect other works on the same (or related) topic to make the state of the art. In the context of image classification, tgit esting the (parametric) scattering networks as part of different neural net architectures (other than those presented in the references below) might be an idea. Most important, it would be crucial to understand the strengths and the weaknesses of this model compared with standard CNN architectures (e.g. ResNet). Is it a valid alternative to standard CNNs on small image datasets ? With what kind of images does it work better ?

## Références

- Shanel Gauthier, Benjamin Thérien, Laurent Alsène-Racicot, Irina Rish, Eugene Belilovsky, Michael Eickenberg, and Guy Wolf. Parametric scattering networks. arXiv preprint arXiv :2107.09539, 2021.
- Edouard Oyallon, Sergey Zagoruyko, Gabriel Huang, Nikos Komodakis, Simon Lacoste-Julien, Matthew Blaschko, and Eugene Belilovsky. Scattering networks for hybrid representation learning. IEEE transactions on pattern analysis and machine intelligence, 41(9) :2208–2221, 2018.
- Laurent Sifre and Stéphane Mallat. Rotation, scaling and deformation invariant scattering for texture discrimination. In Proceedings of the IEEE conference on computer vision and pattern recognition, pages 1233–1240, 2013.
