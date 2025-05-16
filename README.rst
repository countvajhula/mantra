.. image:: https://github.com/countvajhula/mantra/actions/workflows/test.yml/badge.svg
    :target: https://github.com/countvajhula/mantra/actions

.. image:: https://coveralls.io/repos/github/countvajhula/mantra/badge.svg?branch=master
    :target: https://coveralls.io/github/countvajhula/mantra?branch=master

.. image:: https://melpa.org/packages/mantra-badge.svg
    :alt: MELPA
    :target: https://melpa.org/#/mantra

.. image:: https://stable.melpa.org/packages/mantra-badge.svg
    :alt: MELPA Stable
    :target: https://stable.melpa.org/#/mantra

mantra
===========
Parse and compose keyboard activity in Emacs.

Mantra allows you to define "regex"-like patterns on your keyboard activity in terms of start, stop, and abort conditions, together with parsers that transform this keyboard activity (within the context of the surrounding Emacs environment) into arbitrary structured data.

The pattern conditions could be anything, not only based on the key sequence typed. Likewise, the parsers are defined in terms of mapping and composing parsed key sequences within the context of the full surrounding Emacs environment at each step, so that the parsed result could also be just about anything in the context of your activity.

Whenever one of the defined patterns is encountered, the corresponding parser records the structured data (by default, simply the matching sequence of keystrokes) and publishes it using a basic pub/sub system for additional handling by any subscribers you define. Higher levels of parsing (e.g., "record either buffer or window configuration changes, and only when I'm in such-and-such project path") may be achieved by subscribing to these primitive parsers and publishing fresh events if the desired conditions over these primitive parsers are met.

Mantra is purely syntax, without semantics. It does not bind key sequences to commands or even know what commands parsed sequences may be bound to. The patterns and parsers are defined by you and may be associated with any actions that you see fit to perform, independently of any configured keybindings for these key sequences.

This package could conceivably be used to implement packages resembling yasnippet, evil-repeat, Emacs's keyboard macro ring, Evil jumps and changes, winner mode, and much more.

Installation
------------

Mantra is not on a package archive such as `MELPA <https://melpa.org/>`_ yet, but you can install it using `Straight.el <https://github.com/radian-software/straight.el>`_ (or `Elpaca <https://github.com/progfolio/elpaca>`_) by putting this somewhere in your :code:`.emacs.d`:

.. code-block:: elisp

  (use-package mantra
    :straight
    (mantra
      :type git
      :host github
      :repo "countvajhula/mantra"))

Further Reading
---------------

This package is informed by the perspective developed in `A Vimlike Fluency <https://countvajhula.com/2021/01/21/vim-tip-of-the-day-a-series/>`_, especially:

- `Living the High Life <https://countvajhula.com/2021/02/02/vim-tip-of-the-day-living-the-high-life/>`_
- `Saying More (Macros) <https://countvajhula.com/2021/02/08/vim-tip-of-the-day-saying-more-macros/>`_
- `Going Places <https://countvajhula.com/2021/01/30/vim-tip-of-the-day-going-places/>`_

Non-Ownership
-------------

The freely released, copyright-free work in this repository represents an investment in a better way of doing things called attribution-based economics. Attribution-based economics is based on the simple idea that we gain more by giving more, not by holding on to things that, truly, we could only create because we, in our turn, received from others. As it turns out, an economic system based on attribution -- where those who give more are more empowered -- is significantly more efficient than capitalism while also being stable and fair (unlike capitalism, on both counts), giving it transformative power to elevate the human condition and address the problems that face us today along with a host of others that have been intractable since the beginning. You can help make this a reality by releasing your work in the same way -- freely into the public domain in the simple hope of providing value. Learn more about attribution-based economics at `drym.org <https://drym.org>`_, tell your friends, do your part.
