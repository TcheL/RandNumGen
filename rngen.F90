!******************************************************************************!
!*  This is a program that generate a random number set of specified          *!
!*       statistics distribution.                                             *!
!*                                                                            *!
!*  Author: Tche LIU          Email: seistche@gmail.com                       *!
!*  Copyright (C) Tche, 2018. All Rights Reserved.                            *!
!******************************************************************************!

!===============================================================================

!#define DoublePrecision
#define LLS 128
#define LSS 20

!===============================================================================

module Para_mod

  implicit none
  public

#ifndef DoublePrecision
  integer, parameter :: MK = 4
#else
  integer, parameter :: MK = 8
#endif
  real(kind = MK), parameter :: pi = 3.1415926535897932_MK

  logical :: IsVerbose, IsSilent
  character(len = :), allocatable :: ErrorPath
  character(len = :), allocatable :: ErrorInfo

  character(len = :), allocatable :: NameDstrbt
  real(kind = MK) :: unia, unib
  real(kind = MK) :: norMu, norSigma

  real(kind = MK) :: MulprScale, IncrmValue

  integer :: DecimalNum, Len2Dim(2)

  real(kind = MK), allocatable :: rndOut(:, :)

  contains

    subroutine Para_Init()
      NameDstrbt = 'uni'
      unia = 0.0_MK; unib = 1.0_MK
      MulprScale = 1.0_MK; IncrmValue = 0.0_MK
      DecimalNum = - 1; Len2Dim = 1
      IsVerbose = .true.; IsSilent = .true.
      ErrorPath = '/rngen'; ErrorInfo = ' '
    end subroutine Para_Init

    subroutine Para_ConfigPrint()
      if(IsSilent) return
      write(*, *)
      write(*, '(A)') 'Configure:'
      if(NameDstrbt == 'uni') then
        write(*, '(A, G0)') '  unia       = ', unia
        write(*, '(A, G0)') '  unib       = ', unib
      else if(NameDstrbt == 'nor') then
        write(*, '(A, G0)') '  norMu      = ', norMu
        write(*, '(A, G0)') '  norSigma   = ', norSigma
      end if
      write(*, '(A, G0)') '  MulprScale = ', MulprScale
      write(*, '(A, G0)') '  IncrmValue = ', IncrmValue
      if(DecimalNum >= 0) &
        & write(*, '(A, G0)') '  DecimalNum = ', DecimalNum
      write(*, *)
    end subroutine Para_ConfigPrint

    subroutine Para_ErrorExcept(IsError, SupplyString)
      logical, intent(in) :: IsError
      character(len = *), intent(in), optional :: SupplyString
      if(IsError) then
        write(*, *)
        write(*, '(A)') 'ERROR:'
      else
        if(.not. IsVerbose) return
        write(*, *)
        write(*, '(A)') 'WARNING:'
      end if
      write(*, '(A)') '  Error Path: '//ErrorPath
      write(*, '(A)') '  Error Info: '//ErrorInfo
      if(present(SupplyString)) write(*, '(A)') '  >> '//SupplyString
      write(*, *)
      if(IsError) stop 2
    end subroutine Para_ErrorExcept

end module Para_mod

!===============================================================================

module CmdP_mod

  use Para_mod
  implicit none
  private
  
  interface CmdP_ChangePara
    module procedure CmdP_ChangePara_Int
    module procedure CmdP_ChangePara_Int1D
    module procedure CmdP_ChangePara_Real
    module procedure CmdP_ChangePara_Real1D
    module procedure CmdP_ChangePara_Char
    module procedure CmdP_ChangePara_Logical
  end interface CmdP_ChangePara
  
  public :: CmdP_GetProcess, CmdP_PrintHelpInfo

  contains
  
    subroutine CmdP_GetProcess()
      character(len = LSS) :: CmdOpt
      character(len = LLS) :: StrTmp
      integer :: nArg
      real(kind = MK) :: raTmp(2)
      integer :: i
      ErrorPath = ErrorPath//'/CmdP_GetProcess'
      nArg = command_argument_count()
      if(mod(nArg, 2) /= 0) then
        call CmdP_PrintHelpInfo()
        call get_command_argument(1, CmdOpt)
        if(nArg == 1 .and. trim(adjustl(CmdOpt)) == '-h') then
          stop 0
        else
          write(StrTmp, '(A, I0, A)') 'It is not allowed that the number of'// &
            & ' command arguments ', nArg, ' is not even.'
          ErrorInfo = trim(adjustl(StrTmp))
          call Para_ErrorExcept(.true.)
        end if
      end if
      do i = 1, nArg, 2
        call get_command_argument(i, CmdOpt)
        select case(trim(adjustl(CmdOpt)))
          case('-uni')
            NameDstrbt = 'uni'
            call CmdP_ChangePara(raTmp, i + 1)
            unia = raTmp(1); unib = raTmp(2)
          case('-nor')
            NameDstrbt = 'nor'
            call CmdP_ChangePara(raTmp, i + 1)
            norMu = raTmp(1); norSigma = raTmp(2)
          case('-m')
            call CmdP_ChangePara(MulprScale, i + 1)
          case('-i')
            call CmdP_ChangePara(IncrmValue, i + 1)
          case('-p')
            call CmdP_ChangePara(DecimalNum, i + 1)
          case('-d')
            call CmdP_ChangePara(Len2Dim, i + 1)
          case('-v')
            call CmdP_ChangePara(IsVerbose, i + 1)
          case('-s')
            call CmdP_ChangePara(IsSilent, i + 1)
          case default
            call CmdP_PrintHelpInfo()
            write(StrTmp, '(A, G0, A)') 'Except when handling the #', &
              & i, ' command line option.'
            ErrorInfo = trim(adjustl(StrTmp))
            call Para_ErrorExcept(.true., 'Unrecognized command line option: '// &
              & trim(adjustl(CmdOpt)))
        end select
      end do
      ErrorPath = ErrorPath(:index(ErrorPath, '/', .true.) - 1); ErrorInfo = ' '
    end subroutine CmdP_GetProcess
  
    subroutine CmdP_ChangePara_Int(Var, ith)
      integer, intent(out) :: Var
      integer, intent(in) :: ith
      character(len = LLS) :: CmdStr
      call get_command_argument(ith, CmdStr)
      read(CmdStr, *) Var
    end subroutine CmdP_ChangePara_Int
    subroutine CmdP_ChangePara_Int1D(Var, ith)
      integer, intent(out) :: Var(:)
      integer, intent(in) :: ith
      character(len = LLS) :: CmdStr
      character(len = LLS) :: StrTmp
      call get_command_argument(ith, CmdStr)
      ErrorPath = ErrorPath//'/CmdP_ChangePara_Int1D'
      write(StrTmp, '(A, G0, A)') 'Except when handling the #', &
        & ith, ' command line option.'
      if(CmdP_SegmNumInString(',', CmdStr, 1) /= size(Var)) then
        ErrorInfo = trim(adjustl(StrTmp))
        call Para_ErrorExcept(.true., 'Not enough parameter number.')
      else
        read(CmdStr, *) Var
      end if
      ErrorPath = ErrorPath(:index(ErrorPath, '/', .true.) - 1); ErrorInfo = ' '
    end subroutine CmdP_ChangePara_Int1D
    subroutine CmdP_ChangePara_Real(Var, ith)
      real(kind = MK), intent(out) :: Var
      integer, intent(in) :: ith
      character(len = LLS) :: CmdStr
      call get_command_argument(ith, CmdStr)
      read(CmdStr, *) Var
    end subroutine CmdP_ChangePara_Real
    subroutine CmdP_ChangePara_Real1D(Var, ith)
      real(kind = MK), intent(out) :: Var(:)
      integer, intent(in) :: ith
      character(len = LLS) :: CmdStr
      character(len = LLS) :: StrTmp
      call get_command_argument(ith, CmdStr)
      ErrorPath = ErrorPath//'/CmdP_ChangePara_Real1D'
      write(StrTmp, '(A, G0, A)') 'Except when handling the #', &
        & ith, ' command line option.'
      if(CmdP_SegmNumInString(',', CmdStr, 1) /= size(Var)) then
        ErrorInfo = trim(adjustl(StrTmp))
        call Para_ErrorExcept(.true., 'Not enough parameter number.')
      else
        read(CmdStr, *) Var
      end if
      ErrorPath = ErrorPath(:index(ErrorPath, '/', .true.) - 1); ErrorInfo = ' '
    end subroutine CmdP_ChangePara_Real1D
    subroutine CmdP_ChangePara_Char(Var, ith)
      character(len = :), allocatable, intent(out) :: Var
      integer, intent(in) :: ith
      character(len = LLS) :: CmdStr
      call get_command_argument(ith, CmdStr)
      Var = trim(adjustl(CmdStr))
    end subroutine CmdP_ChangePara_Char
    subroutine CmdP_ChangePara_Logical(Var, ith)
      Logical, intent(out) :: Var
      integer, intent(in) :: ith
      character(len = LLS) :: CmdStr
      call get_command_argument(ith, CmdStr)
      if(trim(adjustl(CmdStr)) == 'T') then
        Var = .true.
      else
        Var = .false.
      end if
    end subroutine CmdP_ChangePara_Logical
  
    recursive function CmdP_SegmNumInString(OneChar, String, iCall) result(Num)
      character, intent(in) :: OneChar
      character(len = *), intent(in) :: String
      integer, intent(in) :: iCall
      integer :: Num, ipos
      if(iCall == 1) then
        ErrorPath = ErrorPath//'/CmdP_SegmNumInString'
        ErrorInfo = 'Except when handling the substring "'// &
          & trim(adjustl(String))//'"'
      end if
      ipos = index(String, OneChar)
      if(ipos == 0) then
        if(len_trim(String) == 0) &
          & call Para_ErrorExcept(.false., 'Some segmentation is null.')
        Num = 1
      elseif(ipos == 1) then
        call Para_ErrorExcept(.false., 'Some segmentation is null.')
        Num = CmdP_SegmNumInString(OneChar, String(ipos + 1:), iCall + 1) + 1
      else
        Num = CmdP_SegmNumInString(OneChar, String(ipos + 1:), iCall + 1) + 1
      end if
      if(iCall == 1) then
        ErrorPath = ErrorPath(:index(ErrorPath, '/', .true.) - 1)
        ErrorInfo = ' '
      end if
    end function CmdP_SegmNumInString
    
    subroutine CmdP_PrintHelpInfo()
      if(.not. IsVerbose) return
      write(*, *)
      write(*, '(A)') 'Usage: rngen [-v IsVerbose] [-s IsSilent]'// &
        & ' [ -uni unia,unib | -nor norMu,norSigma ] [-m MulprScale]'// &
        & ' [-i IncrmValue] [-p DecimalNum] [-d Len2Dim]'
      write(*, '(A)') '  [IsVerbose     ]: print verbose warning and help'// &
        & ' information (T) or not (F).'
      write(*, '(A)') '  [IsSilent      ]: print configure parameter (F)'// &
        & ' or not (T).'
      write(*, '(A)') '  [unia,unib     ]: uniform distribution on the'// &
        & ' interval [unia, unib].'
      write(*, '(A)') '  [norMu,norSigma]: normal distribution with mean'// &
        & ' norMu and variance norSigma.'
      write(*, '(A)') '  [MulprScale    ]: original random number'// &
        & ' multiplied by MulprScale to scale.'
      write(*, '(A)') '  [IncrmValue    ]: intermediary random number'// &
        & ' added by IncrmValue to shift.'
      write(*, '(A)') '  [DecimalNum    ]: number of the decimal places'// &
        & ' to be reserved.'
      write(*, '(A)') '  [Len2Dim       ]: dimension lengths of 2-D'// &
        & ' output array: dim1_len,dim2_len'
      write(*, *)
      write(*, '(A)') 'rngen version 1.2 by: Tche LIU, USTC'
      write(*, '(A)') '  [Email]: seistche@gmail.com'
      write(*, '(A)') '  [Date ]: 2020-6-14'
      write(*, *)
    end subroutine CmdP_PrintHelpInfo
  
end module CmdP_mod

!===============================================================================

module Rand_mod

#ifdef IFORT
  use IFPORT
#endif
  use Para_mod, only : MK, pi, NameDstrbt, unia, unib, norMu, norSigma
  implicit none
  public

  integer, parameter, private :: MIK = selected_int_kind(10)

  contains

    subroutine Rand_InitSeed()
      ! More details refer to http://fcode.cn/guide-96-1.html
      integer :: NSeed, PID
      integer(MIK) :: tSecond
      integer, allocatable :: Seed(:)
      integer :: i
      call random_seed(size = NSeed)
      allocate(Seed(NSeed))
      call system_clock(tSecond)
      PID = getpid()
      tSecond = ieor(tSecond, int(PID, kind(tSecond)))
      ! Seed(1) = Rand_LCG(tSecond)
      ! do i = 2, NSeed, 1
      !   Seed(i) = Rand_LCG(Seed(i - 1))
      ! end do
      Seed = Rand_LCG(tSecond)
      call random_seed(put = Seed)
    end subroutine Rand_InitSeed

    function Rand_LCG(x) result(xNext)
      integer(MIK), intent(in) :: x
      integer(MIK) :: xPrep
      integer :: xNext
      xPrep = mod(x*1664525_MIK + 1013904223_MIK, 4294967296_MIK)
      xNext = int(mod(xPrep, int(huge(0), MIK)), kind(0))
    end function Rand_LCG
    
    subroutine Rand_Distribut(rnd)
      real(kind = MK), intent(out) :: rnd
      if(NameDstrbt == 'uni') then
        call Rand_UniDstrbt(rnd)
      else if(NameDstrbt == 'nor') then
        call Rand_NorDstrbt(rnd)
      end if
    end subroutine Rand_Distribut

    subroutine Rand_UniDstrbt(rnd)
      real(kind = MK), intent(out) :: rnd
      call random_number(rnd)
      rnd = (unib - unia)*rnd + unia
    end subroutine Rand_UniDstrbt

    subroutine Rand_NorDstrbt(rnd)
      ! Box-Muller method
      real(kind = MK), intent(out) :: rnd
      real(kind = MK) :: u1, u2
      logical, save :: IsGen = .false.
      call random_number(u1)
      call random_number(u2)
      if(IsGen) then
        rnd = sqrt( - 2.0_MK*log(u1))*cos(2.0*pi*u2)
      else
        rnd = sqrt( - 2.0_MK*log(u1))*sin(2.0*pi*u2)
      end if
      IsGen = .not. IsGen
      rnd = norMu + norSigma*rnd
    end subroutine Rand_NorDstrbt

end module Rand_mod

!===============================================================================

program RandNumGen

  use Para_mod
  use CmdP_mod
  use Rand_mod
  implicit none

  integer :: i, j
  
  call Para_Init()
  call CmdP_GetProcess()
  call Para_ConfigPrint()

  allocate(rndOut(Len2Dim(1), Len2Dim(2)))
  call Rand_InitSeed()
  do i = 1, Len2Dim(1), 1
    do j = 1, Len2Dim(2), 1
      call Rand_Distribut(rndOut(i, j))
    end do
  end do
  rndOut = rndOut*MulprScale + IncrmValue

  call Output(rndOut)
  deallocate(rndOut)

  contains

    subroutine Output(dOut)
      real(kind = MK) :: dOut(:, :)
      character(len = LSS) :: fmtstr
      if(DecimalNum < 0) then
        write(fmtstr, '(A, G0, A)') '(', Len2Dim(2), '(2X, G0))'
      else
        write(fmtstr, '(A, G0, A, G0, A, G0, A)') '(', Len2Dim(2), '(1X, F', &
          & int(log10(maxval(dOut))) + DecimalNum + 3, '.', DecimalNum, '))'
      end if
      do i = 1, Len2Dim(1), 1
        write(*, fmtstr) (dOut(i, j), j = 1, Len2Dim(2), 1)
      end do
    end subroutine Output

end program RandNumGen

